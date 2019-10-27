{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}

module WumberTest.ConstraintSolver where

import Lens.Micro
import Linear.Metric
import Linear.V2
import Linear.V3
import Test.QuickCheck
import Text.Printf

import Debug.Trace

import Wumber.Constraint
import Wumber.ConstraintSolver


newtype UnitInterval a = UnitInterval a deriving Show
instance Arbitrary (UnitInterval N) where
  arbitrary = UnitInterval <$> choose (-1, 1)


instance Arbitrary CVal where arbitrary = CConst <$> arbitrary

instance Arbitrary (V2 N) where
  arbitrary = V2 <$> arbitrary <*> arbitrary

instance Arbitrary (V3 N) where
  arbitrary = V3 <$> arbitrary <*> arbitrary <*> arbitrary


-- | A system is solvable iff it converges to error below the epsilon and hasn't
--   exhausted its iteration count.
solvable :: Int -> N -> Constrained a -> Property
solvable n ε m = counterexample (show (v, n', xs)) $ v <= ε && n' > 0
  where (_, v, n', xs) = solve n ε m


solve_ε = 1e-6


prop_uni_linear :: N -> NonZero N -> CVal -> CVal -> Property
prop_uni_linear x (NonZero m) b y =
  solvable 100 solve_ε do v <- var x
                          v * CConst m + b =-= y


prop_uni_quadratic :: NonNegative N -> NonNegative N -> Property
prop_uni_quadratic (NonNegative x) (NonNegative y) =
  solvable 100 solve_ε do
    v <- var x
    v*v =-= CConst y


prop_v2dist :: V2 N -> NonNegative N -> Property
prop_v2dist vec (NonNegative d) =
  solvable 100 solve_ε do
    v <- vars vec
    norm v =-= CConst d


prop_v2joint_lt :: V2 N -> NonNegative N -> Property
prop_v2joint_lt vec (NonNegative d) =
  solvable 100 solve_ε do
    v <- vars vec
    v^._x  <-= v^._y
    norm v =-= CConst d

prop_v2joint_eq :: V2 N -> NonNegative N -> Property
prop_v2joint_eq vec (NonNegative d) =
  solvable 100 solve_ε do
    v <- vars vec
    v^._x  =-= v^._y
    norm v =-= CConst d


prop_v3dist :: V3 N -> NonNegative N -> Property
prop_v3dist vec (NonNegative d) =
  solvable 100 solve_ε do
    v <- vars vec
    norm v =-= CConst d


return []
runTests = $quickCheckAll
