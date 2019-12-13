{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- | Symbolic derivatives for 'SymMath' expressions.
module Wumber.SymDerivative where


import Data.IntMap (IntMap, fromList)

import Wumber.MathFn
import Wumber.Numeric
import Wumber.SymExpr
import Wumber.SymMath

import qualified Wumber.BitSet as BS


-- TODO
-- Generalize 'derivative'; for non-differentiable types, we can always fall
-- back to delta/epsilon stuff.


-- | A symbolic derivative of the specified quantity with respect to a variable.
derivative :: (Delta a, SymMathC MathFn a)
           => VarID -> SymMath MathFn a -> SymMath MathFn a
derivative v (Math s) = d s
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

        d (SymF Rem [x, y] _)  = math x `quot` math y * d' (math x / math y)
        d (SymF Mod [x, y] _)  = math x `quot` math y * d' (math x / math y)

        -- Uncontroversial implementations
        d (SymF IfNN [c, x, y] _) = apply IfNN [math c, d x, d y]
        d (SymF Upper [x, y] _)   = d' (apply IfNN [math x - math y, d x, d y])
        d (SymF Lower [x, y] _)   = d' (apply IfNN [math x - math y, d y, d x])

        d (SymF Atan2 [x, y] _) =
          (math y * d x - math x * d y) / (math x ** 2 + math y ** 2)

        d (SymF Add xs _) = sum $ map d xs
        d (SymF Mul xs _) =
          sum [product (d x : map math (filter (/= x) xs)) | x <- xs]

        d (SymF RPow [e, x] _) = d x * math e * math x ** (math e - 1)
        d (SymF Pow [x, e] _)  = d x * math e * math x ** (math e - 1)

        d (SymF Log [x] _)     = d x / math x
        d (SymF Exp [x] _)     = d x * exp (math x)
        d (SymF Negate [x] _)  = negate (d x)
        d (SymF Recip [x] _)   = negate (d x / math x ** 2)

        d (SymF Sin [x] _)     = cos (math x) * d x
        d (SymF Cos [x] _)     = negate (sin (math x)) * d x
        d (SymF Tan [x] _)     = d' (sin (math x) / cos (math x))
        d (SymF Sinh [x] _)    = cosh (math x) * d x
        d (SymF Cosh [x] _)    = negate (sinh (math x)) * d x
        d (SymF Tanh [x] _)    = d' (sinh (math x) / cosh (math x))

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
vector_derivative :: (Delta a, SymMathC MathFn a, Functor v, SymVars v)
                  => SymMathV v MathFn a -> v (SymMath MathFn a)
vector_derivative (SymMathV f) = fmap (flip derivative f) var_ids


-- | A Jacobian "matrix" of all variables that appear in a series of syms.
jacobian :: (Delta a, SymMathC MathFn a)
         => [SymMath MathFn a] -> [IntMap (SymMath MathFn a)]
jacobian ss = map (\s -> fromList $ map (\v -> (v, derivative v s)) vs) ss
  where vs = BS.toList $ BS.unions (map (vars_in . unMath) ss)
