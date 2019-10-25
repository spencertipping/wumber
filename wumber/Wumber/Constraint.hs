{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Wumber.Constraint where

import Data.Array.Unboxed


type VarID              = Int
type ConstrainedVar     = Double
type ConstraintSolution = UArray VarID Double

data Constraint = HardConstant VarID Double
                | HardOffset   VarID Double VarID
                | SoftCostFn   [VarID] ([ConstrainedVar] -> Double)


ε = 1e-8


evaluate :: [Constraint] -> ConstraintSolution -> Double
evaluate cs vs = sum $ map (eval_one vs) cs


gradients :: [Constraint] -> ConstraintSolution -> UArray VarID Double
gradients cs vs = array (0, 0) []
