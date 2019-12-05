{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell #-}

-- NOTE
-- The code will still compile and appear to work without this, but 'negate' and
-- 'recip' will loop forever. Remove at your peril.
{-# LANGUAGE NegativeLiterals #-}

{-# OPTIONS_GHC -funbox-strict-fields -Wincomplete-patterns #-}


-- | Symbolic expressions, optimized for fast computer algebra. Expressions are
--   stored in n-ary S-expression form and very little expansion or
--   normalization happens by default.
--
--   Each node tracks several things inline for fast access:
--
--   1. Its full 'BitSet' of variables
--   2. A lazy 'Fingerprint' uniquely identifying it
--   3. Its tree complexity, which is the total number of nodes underneath it

module Wumber.SymExpr (
  Sym,
  val,
  var
) where


import Data.Foldable (foldl', toList)
import GHC.Generics  (Generic)
import Lens.Micro.TH (makeLenses)

import Wumber.Fingerprint

import qualified Data.Vector   as V
import qualified Wumber.BitSet as BS


-- | A symbolic expression whose functions are of type @f@ and whose terminal
--   constants are of type @a@. Don't construct these by hand; instead, use
--   generators like 'var' and 'val', and Haskell math operators.
data Sym f a = SymV !VarID
             | SymC !a
             | SymF { _sym_fn   :: !f,
                      _sym_vars :: BS.BitSet,
                      _sym_id   :: Fingerprint,
                      _sym_size :: !Int,
                      _sym_args :: !(V.Vector (Sym f a)) }
  deriving (Generic)

type VarID = Int

instance (Fingerprintable f, Fingerprintable a) =>
         Fingerprintable (Sym f a) where
  fingerprint (SymV v) = binary_fingerprint (False, binary_fingerprint v)
  fingerprint (SymC c) = binary_fingerprint (True,  fingerprint c)
  fingerprint (SymF _ _ i _ _) = i

instance (Eq f, Eq a) => Eq (Sym f a) where
  SymV i == SymV j = i == j
  SymC a == SymC b = a == b
  SymF _ _ i _ _ == SymF _ _ j _ _ = i == j
  _ == _ = False


makeLenses ''Sym


-- | The class of functions that can be applied to symbolic arguments, yielding
--   another symbolic argument. Symbolic application often just amounts to
--   creating a new node, but some operations, particularly associative and/or
--   commutative ones, will flatten the tree when possible.
class SymbolicApply f where sym_apply :: f -> [Sym f a] -> Sym f a


-- | Apply a function to symbolic quantities with constant folding.
sym_apply_fold :: (Fingerprintable f, Fingerprintable (Sym f a), ValApply f a)
               => f -> [Sym f a] -> Sym f a
sym_apply_fold f xs
  | all (BS.is_empty . vars_in) xs = val (val_apply f (map eval_constant xs))
  | otherwise                      = sym_apply_cons f xs


-- | This function conses a new tree node. This is the way you should implement
--   'sym_apply' if no algebraic rules apply, and if constant folding isn't
--   possible/desirable.
sym_apply_cons :: (Fingerprintable f, Fingerprintable (Sym f a))
               => f -> [Sym f a] -> Sym f a
sym_apply_cons = sfn


class ValApply f a where val_apply :: f -> [a] -> a

instance ValApply (a -> a) a where
  val_apply f [x] = f x
  val_apply _ xs  = error $ "wrong arity for unary fn: " ++ show (length xs)

instance ValApply (a -> a -> a) a where
  val_apply f [x, y] = f x y
  val_apply _ xs     = error $ "wrong arity for binary fn: " ++ show (length xs)


eval_constant :: ValApply f a => Sym f a -> a
eval_constant (SymV i) = error $ "unexpected variable in constexpr " ++ show i
eval_constant (SymC x) = x
eval_constant (SymF f _ _ _ xs) = val_apply f (map eval_constant $ V.toList xs)


val :: a -> Sym f a
val = SymC

var :: VarID -> Sym f a
var = SymV

val_of :: ValApply f a => Sym f a -> Maybe a
val_of s | BS.is_empty (vars_in s) = Just $ eval_constant s
         | otherwise               = Nothing


sfn :: (Foldable l, Functor l, Fingerprintable f, Fingerprintable (Sym f a))
    => f -> l (Sym f a) -> Sym f a
sfn f xs = SymF f (BS.unions $ fmap vars_in xs)
                  (tree_fingerprint [fingerprint f,
                                     tree_fingerprint $ fmap fingerprint xs])
                  (1 + sum (fmap tree_size xs))
                  (V.fromList $ toList xs)


tree_size :: Sym f a -> Int
tree_size (SymV _) = 1
tree_size (SymC _) = 1
tree_size (SymF _ _ _ s _) = s


vars_in :: Sym f a -> BS.BitSet
vars_in (SymV i) = BS.singleton i
vars_in (SymC _) = BS.empty
vars_in (SymF _ v _ _ _) = v
