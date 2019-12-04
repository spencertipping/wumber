{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- | TODO
--   Actually implement this. Right now it's @(f(a + δ) - f(a - δ)) / 2δ@, which
--   works fine for a lot of things but isn't optimal.
module Wumber.SymbolicDerivative where


import Data.IntMap (IntMap, fromList)
import Data.IntSet (unions, toList)

import Wumber.Numeric
import Wumber.Symbolic


-- | A symbolic derivative (it isn't) of the specified quantity with respect to
--   a variable.
derivative :: (Delta a, AlgConstraints f a) => Sym f a -> VarID -> Sym f a
derivative s v = (upper - lower) / (2 * δv)
  where δv    = val (δ 1)
        upper = s //= [(v, var v + δv)]
        lower = s //= [(v, var v - δv)]


-- | The symbolic derivative of a vector quantity, as a vector.
vector_derivative :: (Delta a, AlgConstraints f a, Functor v, SymVars v)
                  => SymV v f a -> v (Sym f a)
vector_derivative f = fmap (derivative (unSymV f)) var_ids


-- | A Jacobian "matrix" of all variables that appear in a series of syms.
jacobian :: (Delta a, AlgConstraints f a) => [Sym f a] -> [IntMap (Sym f a)]
jacobian ss = map (\s -> fromList $ map (\v -> (v, derivative s v)) vs) ss
  where vs = toList $ unions (map vars_in ss)
