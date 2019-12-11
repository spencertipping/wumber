{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}

{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | Functions to reduce 'EquationSystem's to concrete values.
module Wumber.EquationSolve where


import Data.IntMap (IntMap)

import Wumber.EquationSystem
import Wumber.Numeric
import Wumber.SymExpr
import Wumber.SymJIT
import Wumber.SymMath

import qualified Data.IntMap   as IM
import qualified Wumber.BitSet as BS


-- | Reduces a system of equations to concrete values using a mixture of
--   algebraic and numerical methods.
solve :: SymMathC f R => IntMap R -> EquationSystem f R -> IntMap R
solve ivs _ = error "TODO"


-- | Minimizes the solution error for the specified cost function by delegating
--   to the GSL BFGS minimizer.
minimize :: SymMathC f R => IntMap R -> SymMath f R -> IntMap R
minimize ivs _ = error "TODO"
