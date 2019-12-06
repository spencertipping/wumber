{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

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
--   code. Instead, import @Wumber@ to get relevant @Show@ instances and other
--   helpful interfacing.

module Wumber.SymExpr (
  Sym(..),
  SymMeta,
  VarID,
  sym_val,
  sym_var,
  sym_val_of,
  vars_in,
  amb,
  tree_size,
  operands,
  profile,

  SymbolicApply(..),
  sym_apply_foldwith,
  sym_apply_fold,
  sym_apply_cons,

  ProfileApply(..),
  NoProfiles,

  ValApply(..),
  sym_eval,
) where


import Data.Binary   (Binary)
import Data.Foldable (foldl', toList)
import GHC.Generics  (Generic)

import Wumber.Fingerprint

import qualified Data.Vector   as V
import qualified Wumber.BitSet as BS


-- | A single node of a symbolic expression tree whose terminal values are of
--   type @a@, whose functions are of type @f@, and which uses values of type
--   @p@ as profiles (profiles are used by @f@ to optimize pattern matching; see
--   below).
--
--   Don't construct 'Sym' instances by hand; instead, use 'sym_var', 'sym_val',
--   and 'SymbolicApply'. This will build consistent 'SymMeta' objects for you.
--
--   'Sym's compare first using the profile value, then using the fingerprint
--   within that. This creates properties we can use in 'Wumber.AlgebraicSymFn'.

data Sym p f a = SymV !VarID
               | SymC !a
               | SymF { _sym_fn   :: !f,
                        _sym_args :: !(V.Vector (Sym p f a)),
                        _sym_meta :: !(SymMeta p) }
  deriving (Show, Generic)


-- | The way 'Sym' quantities refer to variables. For now this is 'Int' so that
--   we can use 'BitSet's to track dependencies.
type VarID = Int

-- | Metadata stored on each branching 'Sym' node for optimization purposes.
--   'SymMeta' values are opaque outside of this module.
data SymMeta p = SM { _sm_vars :: BS.BitSet,
                      _sm_id   :: Fingerprint,
                      _sm_prof :: !p,
                      _sm_size :: !Int }
  deriving (Show, Eq, Ord, Generic, Binary)


instance Foldable (Sym p f) where
  foldr _ x (SymV _)      = x
  foldr f x (SymC y)      = f y x
  foldr f x (SymF _ xs _) = foldr (flip $ foldr f) x xs

instance (Fingerprintable f, Fingerprintable a) =>
         Fingerprintable (Sym p f a) where
  fingerprint (SymV v) = binary_fingerprint (False, binary_fingerprint v)
  fingerprint (SymC c) = binary_fingerprint (True,  fingerprint c)
  fingerprint (SymF _ _ (SM _ i _ _)) = i

instance (Eq f, Eq a) => Eq (Sym p f a) where
  SymV i                == SymV j                = i == j
  SymC a                == SymC b                = a == b
  SymF _ _ (SM _ i _ _) == SymF _ _ (SM _ j _ _) = i == j
  _                     == _                     = False

instance (Ord f, Ord a, Ord p, ProfileApply p f) => Ord (Sym p f a) where
  a@(SymC av) `compare` b@(SymC bv) = compare (profile a, av) (profile b, bv)
  SymC _      `compare` SymV _      = LT
  SymC _      `compare` SymF _ _ _  = LT

  SymV _      `compare` SymC _      = GT
  a@(SymV ai) `compare` b@(SymV bi) = compare (profile a, ai) (profile b, bi)
  SymV _      `compare` SymF _ _ _  = LT

  SymF _ _ _            `compare` SymC _                = GT
  SymF _ _ _            `compare` SymV _                = GT
  SymF _ _ (SM _ i p _) `compare` SymF _ _ (SM _ j q _) = compare (p, i) (q, j)


-- | Promotes a constant into a symbolic value.
sym_val :: a -> Sym p f a
sym_val = SymC

-- | Returns a symbolic quantity referring to the given indexed variable.
sym_var :: VarID -> Sym p f a
sym_var = SymV


-- | If the symbolic expression can be reduced to a constant value, returns
--   'Just' that value. Otherwise returns 'Nothing'.
--
--   Symbolic expressions have constant values exactly when they refer to no
--   variables.

sym_val_of :: ValApply f a => Sym p f a -> Maybe a
sym_val_of s | BS.is_empty (vars_in s) = Just $ sym_eval s
             | otherwise               = Nothing


-- | Reduces constant 'Sym' expressions to their terminal values. This function
--   is used by 'sym_apply_fold'. Throws an error if the 'Sym' depends on
--   unknown quantities (i.e. variables).
sym_eval :: ValApply f a => Sym p f a -> a
sym_eval (SymV i) = error $ "unexpected variable in constexpr " ++ show i
sym_eval (SymC x) = x
sym_eval (SymF f xs _) = val_apply f (map sym_eval $ V.toList xs)


-- | Returns the set of variables referred to by the given tree.
vars_in :: Sym p f a -> BS.BitSet
vars_in (SymV i) = BS.singleton i
vars_in (SymC _) = BS.empty
vars_in (SymF _ _ (SM b _ _ _)) = b


-- | Chooses the simpler of two equivalent representations of the same logical
--   value.
amb :: Sym p f a -> Sym p f a -> Sym p f a
amb a b | tree_size a < tree_size b = a
        | otherwise                 = b

-- | Returns the total number of elements in the given tree: nodes and
--   functions. When algebraic rewriting provides multiple representations, we
--   usually choose the one with the smallest 'tree_size'.
tree_size :: Sym p f a -> Int
tree_size (SymV _) = 1
tree_size (SymC _) = 1
tree_size (SymF _ _ (SM _ _ _ s)) = s


-- | Returns the operands of the given node, or an empty vector if the node is a
--   terminal.
operands :: Sym p f a -> V.Vector (Sym p f a)
operands (SymV _) = V.empty
operands (SymC _) = V.empty
operands (SymF _ v _) = v


-- | Returns the profile of the given node.
profile :: ProfileApply p f => Sym p f a -> p
profile (SymV i)                = prof_var
profile (SymC x)                = prof_val
profile (SymF _ _ (SM _ _ p _)) = p


-- | The class of functions that can be applied to symbolic arguments, yielding
--   another symbolic argument. Symbolic application often just amounts to
--   creating a new node, but some operations, particularly associative and/or
--   commutative ones, will flatten the tree when possible.
class (Fingerprintable a, ProfileApply p f) => SymbolicApply p f a where
  sym_apply :: f -> [Sym p f a] -> Sym p f a

-- | A generalized way to add constant folding to a tree-consing function. We
--   use this generality when we introduce algebraic normalization in
--   'Wumber.AlgebraicSymFn'.
sym_apply_foldwith :: (Fingerprintable f, Fingerprintable (Sym p f a),
                       ProfileApply p f, ValApply f a)
                   => (f -> [Sym p f a] -> Sym p f a)
                   -> f -> [Sym p f a] -> Sym p f a
sym_apply_foldwith cons f xs
  | all (BS.is_empty . vars_in) xs = sym_val (val_apply f (map sym_eval xs))
  | otherwise                      = cons f xs

-- | Apply a function to symbolic quantities with constant folding.
sym_apply_fold = sym_apply_foldwith sym_apply_cons

-- | This function conses a new tree node. This is the way you should implement
--   'sym_apply' if no algebraic rules apply, and if constant folding isn't
--   possible/desirable. (Otherwise you should back into 'sym_apply_fold'.)
sym_apply_cons :: (Fingerprintable f, Fingerprintable (Sym p f a),
                   ProfileApply p f)
               => f -> [Sym p f a] -> Sym p f a
sym_apply_cons f xs = SymF f v (SM b id p s)
  where b  = BS.unions $ map vars_in xs
        id = tree_fingerprint $ fingerprint f : map fingerprint xs
        s  = 1 + sum (map tree_size xs)
        p  = prof_fn f $ map profile xs
        v  = V.fromList xs


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
newtype NoProfiles f = NP () deriving (Show, Eq, Ord)

instance ProfileApply (NoProfiles f) f where
  prof_fn  _ _ = NP ()
  prof_val     = NP ()
  prof_var     = NP ()


-- | The class of functions that can be applied to values of type @a@, yielding
--   another value of type @a@. If your function type implements 'sym_apply'
--   using 'sym_apply_fold', then 'val_apply' will be used to handle constant
--   folding.
class ValApply f a where val_apply :: f -> [a] -> a
