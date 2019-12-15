{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Symbolic derivatives for 'SymMath' expressions.
module Wumber.SymDerivative where


import Data.IntMap (IntMap, fromList)

import Wumber.MathFn
import Wumber.Numeric
import Wumber.SymExpr
import Wumber.SymMath

import qualified Wumber.BitSet as BS


-- | Numeric or symbolic derivative of a quantity, whichever result is less
--   complex.
derivative :: (Delta a, SymDifferentiable f a)
           => VarID -> SymMath f a -> SymMath f a
derivative v s = math $ amb (unMath $ symbolic_derivative v s)
                            (unMath $ numeric_derivative v s)


-- | Derivative calculated using deltas and division. In general this should
--   closely approximate the symbolic derivative, but may produce very large
--   values for non-continuous functions.
numeric_derivative :: (Delta a, SymMathC f a)
                   => VarID -> SymMath f a -> SymMath f a
numeric_derivative v s = (upper - lower) / (2 * δv)
  where δv    = val (δ 1)
        upper = s // [(v, var v + δv)]
        lower = s // [(v, var v - δv)]


-- | The class of function types that provide symbolic differentiation.
class SymMathC f a => SymDifferentiable f a where
  symbolic_derivative :: VarID -> SymMath f a -> SymMath f a


instance SymMathC MathFn a => SymDifferentiable MathFn a where
  symbolic_derivative v s = d' s
    where d' = d . unMath

          d e | not (has_var v e) = val 0
          d (SymV i) | i == v     = val 1

          -- Questionable implementations
          d (SymF Signum _ _)    = val 0
          d (SymF Round _ _)     = val 0
          d (SymF Truncate _ _)  = val 0
          d (SymF Floor _ _)     = val 0
          d (SymF Ceiling _ _)   = val 0
          d (SymF Quot [x, y] _) = val 0
          d (SymF Div  [x, y] _) = val 0

          d (SymF Rem [x, y] _)  = d x - math x `quot` math y * d y
          d (SymF Mod [x, y] _)  = d x - math x `div`  math y * d y

          -- Uncontroversial implementations
          d (SymF IfNN [c, x, y] _) = apply IfNN [math c, d x, d y]
          d (SymF Upper [x, y] _)   = d' (apply IfNN [math x - math y, d x, d y])
          d (SymF Lower [x, y] _)   = d' (apply IfNN [math x - math y, d y, d x])

          d (SymF Atan2 [x, y] _) =
            (math y * d x - math x * d y) / (math x ** 2 + math y ** 2)

          d (SymF Add xs _) = sum $ map d xs
          d (SymF Mul xs _) = sum [product (d x : others x) | x <- xs]
            where others x = [math y | y <- xs, y /= x]

          d (SymF RPow [e, x] _) = d x * math e * math x ** (math e - 1)
          d (SymF Pow [x, e] _)  = d x * math e * math x ** (math e - 1)

          d (SymF Log [x] _)     = d x / math x
          d (SymF Exp [x] _)     = d x * exp (math x)
          d (SymF Negate [x] _)  = negate (d x)
          d (SymF Recip [x] _)   = negate (d x / math x ** 2)

          d (SymF Sin [x] _)     = cos (math x) * d x
          d (SymF Cos [x] _)     = negate (sin (math x)) * d x
          d (SymF Tan [x] _)     = d x / cos (math x) ** 2
          d (SymF Sinh [x] _)    = cosh (math x) * d x
          d (SymF Cosh [x] _)    = negate (sinh (math x)) * d x
          d (SymF Tanh [x] _)    = d x / cosh (math x) ** 2

          d (SymF Asin [x] _)    = d x / sqrt (1 - math x ** 2)
          d (SymF Acos [x] _)    = negate (d x / sqrt (1 - math x ** 2))
          d (SymF Atan [x] _)    = d x / (1 + math x ** 2)
          d (SymF Asinh [x] _)   = d x / sqrt (1 + math x ** 2)
          d (SymF Acosh [x] _)   = d x / sqrt (1 - math x ** 2)
          d (SymF Atanh [x] _)   = d x / (1 - math x ** 2)

          d (SymF Sqr [x] _)     = 2 * math x * d x
          d (SymF Sqrt [x] _)    = 0.5 * d x / sqrt (math x)
          d (SymF Abs [x] _)     = apply IfNN [math x, d x, d' (negate (math x))]


-- | The symbolic derivative of a vector quantity, as a vector.
vector_derivative :: (Delta a, SymDifferentiable f a, Functor v, SymVars v)
                  => SymMathV v f a -> v (SymMath f a)
vector_derivative (SymMathV f) = fmap (flip derivative f) var_ids


-- | A Jacobian "matrix" of all variables that appear in a series of syms.
jacobian :: (Functor t, Foldable t, Delta a, SymDifferentiable f a)
         => t (SymMath f a) -> t (IntMap (SymMath f a))
jacobian ss = fmap (\s -> fromList $ map (\v -> (v, derivative v s)) vs) ss
  where vs = BS.toList $ foldr (\s b -> BS.union b (vars_in s)) BS.empty ss
