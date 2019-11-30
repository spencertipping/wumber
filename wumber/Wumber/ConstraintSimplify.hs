{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | Algebraic constraint simplification. The goal here is to reduce the number
--   of variables by substitution.
module Wumber.ConstraintSimplify where


import Wumber.Constraint
import Wumber.Symbolic
import Wumber.SymbolicAlgebra
