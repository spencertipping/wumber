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
--
--   Don't @import@ this module directly unless you're writing Wumber-internal
--   code. Instead, import @Wumber@ to get relevant @Show@ instances and other
--   helpful interfacing.

module Wumber.SymExpr (
  Sym(..),
  SymMeta,
  VarID,
  val,
  var,
  val_of,
  vars_in,
  tree_size,
  operands,

  SymbolicApply(..),
  sym_apply_fold,
  sym_apply_cons,

  ProfileApply(..),
  NoProfiles,

  ValApply(..),
  eval_constant,
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
--   Don't construct 'Sym' instances by hand; instead, use 'var', 'val', and
--   'SymbolicApply'. This will build consistent 'SymMeta' objects for you.

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

instance (Ord f, Ord a) => Ord (Sym p f a) where
  SymV i `compare` SymV j     = compare i j
  SymV _ `compare` SymC _     = LT
  SymV _ `compare` SymF _ _ _ = LT

  SymC _ `compare` SymV _     = GT
  SymC a `compare` SymC b     = compare a b
  SymC _ `compare` SymF _ _ _ = LT

  SymF _ _ _            `compare` SymV _                = GT
  SymF _ _ _            `compare` SymC _                = GT
  SymF _ _ (SM _ i _ _) `compare` SymF _ _ (SM _ j _ _) = compare i j


-- | Promotes a constant into a symbolic value.
val :: a -> Sym p f a
val = SymC

-- | Returns a symbolic quantity referring to the given indexed variable.
var :: VarID -> Sym p f a
var = SymV


-- | If the symbolic expression can be reduced to a constant value, returns
--   'Just' that value. Otherwise returns 'Nothing'.
--
--   Symbolic expressions have constant values exactly when they refer to no
--   variables.

val_of :: ValApply f a => Sym p f a -> Maybe a
val_of s | BS.is_empty (vars_in s) = Just $ eval_constant s
         | otherwise               = Nothing


-- | Returns the set of variables referred to by the given tree.
vars_in :: Sym p f a -> BS.BitSet
vars_in (SymV i) = BS.singleton i
vars_in (SymC _) = BS.empty
vars_in (SymF _ _ (SM b _ _ _)) = b


-- | Returns the operands of the given node, or an empty vector if the node is a
--   terminal.
operands :: Sym p f a -> V.Vector (Sym p f a)
operands (SymV _) = V.empty
operands (SymC _) = V.empty
operands (SymF _ v _) = v


-- | The class of functions that can be applied to symbolic arguments, yielding
--   another symbolic argument. Symbolic application often just amounts to
--   creating a new node, but some operations, particularly associative and/or
--   commutative ones, will flatten the tree when possible.
class (Fingerprintable a, ProfileApply p f a) => SymbolicApply p f a where
  sym_apply :: f -> [Sym p f a] -> Sym p f a

-- | Apply a function to symbolic quantities with constant folding.
sym_apply_fold :: (Fingerprintable f, Fingerprintable (Sym p f a),
                   ProfileApply p f a, ValApply f a)
               => f -> [Sym p f a] -> Sym p f a
sym_apply_fold f xs
  | all (BS.is_empty . vars_in) xs = val (val_apply f (map eval_constant xs))
  | otherwise                      = sym_apply_cons f xs

-- | This function conses a new tree node. This is the way you should implement
--   'sym_apply' if no algebraic rules apply, and if constant folding isn't
--   possible/desirable. (Otherwise you should back into 'sym_apply_fold'.)
sym_apply_cons :: (Fingerprintable f, Fingerprintable (Sym p f a),
                   ProfileApply p f a)
               => f -> [Sym p f a] -> Sym p f a
sym_apply_cons f xs = SymF f v (SM b id p s)
  where b  = BS.unions $ map vars_in xs
        id = tree_fingerprint $ fingerprint f : map fingerprint xs
        s  = 1 + sum (map tree_size xs)
        p  = prof_fn f $ map prof xs
        v  = V.fromList xs

        prof (SymV i)                = prof_var i
        prof (SymC x)                = prof_val x
        prof (SymF _ _ (SM _ _ p _)) = p


-- | A class that allows functions to store profile values onto 'Sym' quantities
--   they build up. Profiles are structural hashes of a fixed number of layers
--   of 'Sym' expressions; the purpose of this is to rapidly pattern-match
--   against a function, its arity, and metadata about its arguments.
--
--   Implementing profiles is optional; you can use @NoProfiles f a@ if you
--   don't want to go to the trouble.

class ProfileApply p f a | p -> f, p -> a where
  prof_fn  :: f -> [p] -> p
  prof_val :: a -> p
  prof_var :: VarID -> p

-- | A type you can use to bypass profile calculation. If you don't
--   pattern-match against 'Sym' quantities, this probably makes sense.
newtype NoProfiles f a = NP ()

instance ProfileApply (NoProfiles f a) f a where
  prof_fn  _ _ = NP ()
  prof_val _   = NP ()
  prof_var _   = NP ()


-- | The class of functions that can be applied to values of type @a@, yielding
--   another value of type @a@. If your function type implements 'sym_apply'
--   using 'sym_apply_fold', then 'val_apply' will be used to handle constant
--   folding.
--
--   Instances are provided for @a -> a@ and @a -> a -> a@ even though you
--   wouldn't use either of these types for @f@.

class ValApply f a where val_apply :: f -> [a] -> a

instance ValApply (a -> a) a where
  val_apply f [x] = f x
  val_apply _ xs  = error $ "wrong arity for unary fn: " ++ show (length xs)

instance ValApply (a -> a -> a) a where
  val_apply f (x:xs) = foldl f x xs
  val_apply _ []     = error "can't apply binary function to zero arguments"


-- | An internal function that reduces constant 'Sym' expressions to their
--   terminal values. This function is used by 'sym_apply_fold'.
eval_constant :: ValApply f a => Sym p f a -> a
eval_constant (SymV i) = error $ "unexpected variable in constexpr " ++ show i
eval_constant (SymC x) = x
eval_constant (SymF f xs _) = val_apply f (map eval_constant $ V.toList xs)


-- | Returns the total number of elements in the given tree: nodes and
--   functions. When algebraic rewriting provides multiple representations, we
--   usually choose the one with the smallest 'tree_size'.
tree_size :: Sym p f a -> Int
tree_size (SymV _) = 1
tree_size (SymC _) = 1
tree_size (SymF _ _ (SM _ _ _ s)) = s
