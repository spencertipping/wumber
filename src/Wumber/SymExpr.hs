{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -funbox-strict-fields -Wincomplete-patterns #-}


-- | Symbolic expressions, optimized for fast computer algebra. Specifically,
--   'Sym' is the container format for @newtype@s we later use for more specific
--   purposes. Expressions are stored in n-ary S-expression form and very little
--   expansion or normalization happens by default.
--
--   Each node tracks several things inline for fast access:
--
--   1. Its full 'BitSet' of variables
--   2. A lazy 'Fingerprint' uniquely identifying it
--   3. Its tree complexity, which is the total number of nodes underneath it
--
--   Of these, only the tree complexity is calculated eagerly.
--
--   'Sym's provide /O(1)/ equality and comparison after the first invocation.
--   Ordering is delegated to profiles, then fingerprints and/or terminals.
--   Constants sort first, then variables, then function applications.
--
--   Don't @import@ this module directly unless you're writing Wumber-internal
--   code. Instead, use 'Wumber.SymMath' to get a more useful interface.

module Wumber.SymExpr (
  Sym(..),
  SymMeta,
  VarID,
  SymVal(..),
  SymLift(..),
  has_var,
  amb,
  ambs,
  tree_size,
  arity,
  operands,
  descendants,
  profile,
  Rewritable(..),
  (//),
  (//:),
  eval,

  SymbolicApply(..),
  sym_apply_foldwith,
  sym_apply_fold,
  sym_apply_cons,

  ProfileApply(..),
  NoProfiles(..),

  ValApply(..),
) where


import Data.Binary   (Binary)
import Data.Foldable (foldl', toList)
import Data.Int      (Int32)
import Data.IntMap   (IntMap)
import Data.Maybe    (fromJust, fromMaybe, isJust)
import GHC.Generics  (Generic)

import Wumber.Fingerprint
import Wumber.Numeric

import qualified Data.IntMap   as IM
import qualified Wumber.BitSet as BS


-- | A single node of a symbolic expression tree whose terminal values are of
--   type @a@, whose functions are of type @f@, and which uses values of type
--   @p@ as profiles (profiles are used by @f@ to optimize pattern matching; see
--   below).
--
--   Don't construct 'Sym' instances by hand; instead, use 'var', 'val', and
--   'sym_apply_cons'. This will build consistent 'SymMeta' objects for you. (In
--   fact, the 'SymMeta' constructor is hidden specifically for this reason.)
--
--   'Sym's compare first using the profile value, then using the fingerprint
--   within that. Profile-first comparisons make it possible to sort operands in
--   a way that's useful for algebraic operations, if your functions are
--   commutative.

data Sym p f a = SymV !VarID
               | SymC !a
               | SymF { _sym_fn   :: !f,
                        _sym_args :: ![Sym p f a],
                        _sym_meta :: !(SymMeta p) }
  deriving (Generic, Binary)


-- | The way 'Sym' quantities refer to variables. For now this is 'Int' so that
--   we can use 'BitSet's to track dependencies.
type VarID = Int

-- | Metadata stored on each branching 'Sym' node for optimization purposes.
--   'SymMeta' values are opaque outside of this module.
data SymMeta p = SM { _sm_vars  :: BS.BitSet,
                      _sm_id    :: Fingerprint,
                      _sm_prof  :: !p,
                      _sm_size  :: !Int32,
                      _sm_arity :: !Int32 }
  deriving (Show, Eq, Ord, Generic, Binary)


instance (Show f, Show a) => Show (Sym p f a) where
  show (SymV i)      = "v" ++ show i
  show (SymC x)      = show x
  show (SymF f xs _) = "(" ++ show f ++ concatMap (" " ++) (map show xs) ++ ")"

instance Foldable (Sym p f) where
  foldr _ x (SymV _)      = x
  foldr f x (SymC y)      = f y x
  foldr f x (SymF _ xs _) = foldr (flip $ foldr f) x xs

instance (Fingerprintable f, Fingerprintable a) =>
         Fingerprintable (Sym p f a) where
  fingerprint (SymV v) = binary_fingerprint (False, binary_fingerprint v)
  fingerprint (SymC c) = binary_fingerprint (True,  fingerprint c)
  fingerprint (SymF _ _ (SM _ i _ _ _)) = i

instance (Eq f, Eq a) => Eq (Sym p f a) where
  SymV i                  == SymV j                  = i == j
  SymC a                  == SymC b                  = a == b
  SymF _ _ (SM _ i _ _ _) == SymF _ _ (SM _ j _ _ _) = i == j
  _                       == _                       = False

instance (Ord f, Ord a, Ord p, ProfileApply p f) => Ord (Sym p f a) where
  a@(SymC av) `compare` b@(SymC bv) = compare (profile a, av) (profile b, bv)
  SymC _      `compare` SymV _      = LT
  SymC _      `compare` SymF _ _ _  = LT

  SymV _      `compare` SymC _      = GT
  a@(SymV ai) `compare` b@(SymV bi) = compare (profile a, ai) (profile b, bi)
  SymV _      `compare` SymF _ _ _  = LT

  SymF _ _ _              `compare` SymC _                  = GT
  SymF _ _ _              `compare` SymV _                  = GT
  SymF _ _ (SM _ i p _ _) `compare` SymF _ _ (SM _ j q _ _) =
    compare (p, i) (q, j)


-- | Promotes a constant or variable into a symbolic value.
class SymLift a s | s -> a where
  val     :: a -> s
  var     :: VarID -> s
  vars_in :: s -> BS.BitSet

instance SymLift a (Sym p f a) where
  val     = SymC
  var     = SymV
  vars_in = sym_vars_in


-- | The class of symbolic things that can sometimes be reduced to non-symbolic
--   values.
class SymVal s a | s -> a where val_of :: s -> Maybe a
instance SymVal a b => SymVal (Sym p f a) b where
  val_of (SymC x) = val_of x
  val_of _        = Nothing


-- | The class of things that can be rewritten by substituting variables.
class Rewritable a where rewrite :: a -> (BS.BitSet, IntMap a) -> a
instance SymbolicApply p f a => Rewritable (Sym p f a) where
  rewrite v@(SymV i) (mb, mv) = fromMaybe v $ mv IM.!? i
  rewrite v@(SymF f xs (SM b _ _ _ _)) r@(mb, mv)
    | BS.null (BS.intersect mb b) = v
    | otherwise                   = sym_apply f $ map (flip rewrite r) xs
  rewrite x _ = x


infixl 4 //:
infixl 4 //

(//:) :: (SymLift a s, Rewritable s, Eq s) => s -> IntMap s   -> s
(//)  :: (SymLift a s, Rewritable s, Eq s) => s -> [(Int, s)] -> s

v //: xs = v // IM.toList xs
v //  xs = rewrite v (BS.fromList $ map fst xs', IM.fromList xs')
  where xs' = filter (\(a, b) -> b /= var a) xs

eval :: (SymLift a s, SymVal s a, Rewritable s, Eq s) => (VarID -> a) -> s -> a
eval f s = fromJust $ val_of $ s // [(v, val (f v)) | v <- BS.toList (vars_in s)]


-- | Returns the set of variables referred to by the given tree.
sym_vars_in :: Sym p f a -> BS.BitSet
sym_vars_in (SymV i) = BS.singleton i
sym_vars_in (SymC _) = BS.empty
sym_vars_in (SymF _ _ (SM b _ _ _ _)) = b


-- | Returns 'True' if the expression refers to the given variable.
has_var :: SymLift a s => VarID -> s -> Bool
has_var v = BS.member v . vars_in


-- | Chooses the simpler of two equivalent representations of the same logical
--   value.
amb :: Sym p f a -> Sym p f a -> Sym p f a
amb a b | tree_size a < tree_size b = a
        | otherwise                 = b

ambs = foldl1 amb


-- | Returns the total number of elements in the given tree: nodes and
--   functions. When algebraic rewriting provides multiple representations, we
--   usually choose the one with the smallest 'tree_size'.
--
--   prop> tree_size x == length (descendants x)

tree_size :: Sym p f a -> Int
tree_size (SymV _) = 1
tree_size (SymC _) = 1
tree_size (SymF _ _ (SM _ _ _ s _)) = fi s


-- | Returns the number of children of the current node.
--
--   prop> arity x == length (operands x)

arity :: Sym p f a -> Int
arity (SymV _) = 0
arity (SymC _) = 0
arity (SymF _ _ (SM _ _ _ _ a)) = fi a


-- | Returns the operands of the given node, or an empty list if the node is a
--   terminal.
operands :: Sym p f a -> [Sym p f a]
operands (SymV _) = []
operands (SymC _) = []
operands (SymF _ v _) = v


-- | Returns the profile of the given node.
profile :: ProfileApply p f => Sym p f a -> p
profile (SymV i)                  = prof_var
profile (SymC x)                  = prof_val
profile (SymF _ _ (SM _ _ p _ _)) = p


-- | Returns this and all descendants.
descendants :: Sym p f a -> [Sym p f a]
descendants v@(SymV _)      = [v]
descendants c@(SymC _)      = [c]
descendants v@(SymF _ xs _) = v : concatMap descendants xs


-- | The class of functions that can be applied to symbolic arguments, yielding
--   another symbolic argument. Symbolic application often just amounts to
--   creating a new node, but some operations, particularly associative and/or
--   commutative ones, will flatten the tree when possible.
class (Fingerprintable a, ProfileApply p f) => SymbolicApply p f a where
  sym_apply :: f -> [Sym p f a] -> Sym p f a

-- | A generalized way to add constant folding to a tree-consing function. We
--   use this generality when we introduce algebraic normalization in
--   'Wumber.AlgebraicSymFn'.
sym_apply_foldwith cons f xs
  | all isJust vs                            = val $ val_apply f $ map fromJust vs
  | SymF f' xs' _ <- y, f' /= f || xs' /= xs = sym_apply f' xs'
  | otherwise                                = y
  where vs = map val_of xs
        y  = cons f xs

-- | Apply a function to symbolic quantities with constant folding.
sym_apply_fold = sym_apply_foldwith sym_apply_cons

-- | This function conses a new tree node. This is the way you should implement
--   'sym_apply' if no algebraic rules apply, and if constant folding isn't
--   possible/desirable. (Otherwise you should back into 'sym_apply_fold'.)
sym_apply_cons :: (Fingerprintable f, Fingerprintable (Sym p f a),
                   ProfileApply p f)
               => f -> [Sym p f a] -> Sym p f a
sym_apply_cons f xs = SymF f xs (SM b id p (fi s) (fi $ length xs))
  where b  = BS.unions $ map vars_in xs
        id = tree_fingerprint $ fingerprint f : map fingerprint xs
        s  = 1 + sum (map tree_size xs)
        p  = prof_fn f $ map profile xs


-- | A class that allows functions to store profile values onto 'Sym' quantities
--   they build up. Profiles are structural hashes of a fixed number of layers
--   of 'Sym' expressions; the purpose of this is to rapidly pattern-match
--   against a function, its arity, and metadata about its arguments.
--
--   Implementing profiles is optional; you can use @NoProfiles f@ if you don't
--   want to go to the trouble.

class ProfileApply p f | p -> f, f -> p where
  prof_fn  :: f -> [p] -> p
  prof_val :: p
  prof_var :: p

-- | A type you can use to bypass profile calculation. If you don't
--   pattern-match against 'Sym' quantities, this probably makes sense.
newtype NoProfiles f = NP () deriving (Show, Eq, Ord, Generic, Binary)


-- | The class of functions that can be applied to values of type @a@, yielding
--   another value of type @a@. If your function type implements 'sym_apply'
--   using 'sym_apply_fold', then 'val_apply' will be used to handle constant
--   folding.
class ValApply f a where val_apply :: f -> [a] -> a
