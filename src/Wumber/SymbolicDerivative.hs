{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- | TODO
--   Actually implement this. Right now it's @(f(a + δ) - f(a - δ)) / 2δ@, which
--   works fine for a lot of things but isn't optimal.
module Wumber.SymbolicDerivative where


import Data.IntMap (IntMap, fromList)

import Wumber.MathFn
import Wumber.Numeric
import Wumber.SymExpr
import Wumber.SymMath

import qualified Wumber.BitSet as BS


-- | A symbolic derivative (it isn't) of the specified quantity with respect to
--   a variable.
derivative :: (Delta a, SymMathC f a) => SymMath f a -> VarID -> SymMath f a
derivative s v = (upper - lower) / (2 * δv)
  where δv    = val (δ 1)
        upper = s // [(v, var v + δv)]
        lower = s // [(v, var v - δv)]


-- | The symbolic derivative of a vector quantity, as a vector.
vector_derivative :: (Delta a, SymMathC f a, Functor v, SymVars v)
                  => SymMathV v f a -> v (SymMath f a)
vector_derivative (SymMathV f) = fmap (derivative f) var_ids


-- | A Jacobian "matrix" of all variables that appear in a series of syms.
jacobian :: (Delta a, SymMathC f a) => [SymMath f a] -> [IntMap (SymMath f a)]
jacobian ss = map (\s -> fromList $ map (\v -> (v, derivative s v)) vs) ss
  where vs = BS.toList $ BS.unions (map (vars_in . unMath) ss)
