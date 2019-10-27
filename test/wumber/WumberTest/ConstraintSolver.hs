{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}

module WumberTest.ConstraintSolver where

import Test.QuickCheck

import Wumber.Constraint
import Wumber.ConstraintSolver


prop_quadratic :: N -> N -> Property
prop_quadratic x f = abs (f - 1) < 0.5 && x >= 0 ==> solvable 100 do
  v <- var x
  v =-= CConst (x * f)


return []
runTests = $quickCheckAll
