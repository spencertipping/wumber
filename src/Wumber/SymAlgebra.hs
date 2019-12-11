{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -funbox-strict-fields -Wincomplete-patterns #-}

-- | Algebraic simplification/rewriting for 'SymMath' quantities. The most
--   important operation here is 'isolate', which attempts to isolate a variable
--   within an equation.
module Wumber.SymAlgebra (
  isolate,
  Invertible(..)
) where


import Data.Binary  (Binary(..))
import Data.List    (partition)
import GHC.Generics (Generic(..))

import Wumber.MathFn
import Wumber.SymExpr
import Wumber.SymMath

import qualified Wumber.BitSet as BS


-- | @isolate v s@ attempts to solve the implicit equation @s = 0@ for the
--   variable @v@.
isolate :: (Invertible (SymMath f a), SymMathC f a)
        => VarID -> SymMath f a -> Maybe (SymMath f a)
isolate v e = invert v e 0


-- | Things that can provide inversions against values.
class Invertible a where invert :: VarID -> a -> a -> Maybe a


-- TODO
-- If we see a variable shared across multiple subterms, we may still be able to
-- invert by aggressively distributing operations and collapsing terms.

instance (SymVal a a, SymMathC MathFn a) =>
         Invertible (SymMath MathFn a) where
  invert v (Math (SymV i)) b | i == v = Just b
  invert v (Math (SymF f xs _)) b | [x] <- cs = goforit (Math x)
                                  | otherwise = Nothing
    where (cs, ncs) = partition (has_var v) xs
          goforit x = case f of
            Add -> invert v x $ b - apply' Add ncs
            Mul -> invert v x $ b / apply' Mul ncs
            Pow -> invert v (apply' RPow $ reverse xs) b
            RPow -> case xs of
              [p, x] | has_var v x -> invert v (Math x) $ b ** (1 / Math p)
              [p, x] | has_var v p -> invert v (Math p) $ log b / log (Math x)
              _                    -> Nothing

            _ | [] <- ncs, Just g <- invert v f f ->
                invert v x $ Math (sym_apply g [unMath b])
            _ -> Nothing

  invert _ _ _ = Nothing


-- | Some math functions can be directly inverted by other math functions. A
--   'Nothing' reply doesn't mean the function is uninvertible, but rather that
--   its inversion can't be expressed by applying a unary function.
instance Invertible MathFn where
  invert _ Log _    = Just Exp
  invert _ Exp _    = Just Log
  invert _ Sin _    = Just Asin
  invert _ Cos _    = Just Acos
  invert _ Tan _    = Just Atan
  invert _ Asin _   = Just Sin
  invert _ Acos _   = Just Cos
  invert _ Atan _   = Just Tan
  invert _ Sinh _   = Just Asinh
  invert _ Cosh _   = Just Acosh
  invert _ Tanh _   = Just Atanh
  invert _ Asinh _  = Just Sinh
  invert _ Acosh _  = Just Cosh
  invert _ Atanh _  = Just Tanh
  invert _ Negate _ = Just Negate
  invert _ Recip _  = Just Recip
  invert _ Sqrt _   = Just Sqr
  invert _ Sqr _    = Just Sqrt
  invert _ _ _      = Nothing
