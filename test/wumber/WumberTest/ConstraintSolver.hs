{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

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
--
--   TODO: figure out exactly what ε means to the minimize functions from GSL.
solvable :: (Functor f, Show (f N)) => N -> Int -> Constrained (f CVal) -> Property
solvable ε n m = counterexample (show (xs, v, a)) $ v <= sqrt ε
  where (a, xs, cs) = solve ε n m
        v           = eval_all cs xs


solve_ε = 1e-6
iterations = 10000

t = solvable solve_ε iterations


prop_uni_linear :: N -> NonZero N -> CVal -> CVal -> Property
prop_uni_linear x (NonZero m) b y = t do
  v <- var x
  v * CConst m + b =-= y
  return [v]


prop_uni_quadratic :: NonNegative N -> NonNegative N -> Property
prop_uni_quadratic (NonNegative x) (NonNegative y) = t do
  v <- var x
  v*v =-= CConst y
  return [v]


prop_v2dist :: V2 N -> NonNegative N -> Property
prop_v2dist vec (NonNegative d) = t do
  v <- vars vec
  norm v =-= CConst d
  return v


prop_v2joint_lt :: V2 N -> NonNegative N -> Property
prop_v2joint_lt vec (NonNegative d) = t do
  v <- vars vec
  v^._x  <-= v^._y
  norm v =-= CConst d
  return v

prop_v2joint_eq :: V2 N -> NonNegative N -> Property
prop_v2joint_eq vec (NonNegative d) = t do
  v <- vars vec
  v^._x  =-= v^._y
  norm v =-= CConst d
  return v


prop_v3dist :: V3 N -> NonNegative N -> Property
prop_v3dist vec (NonNegative d) = t do
  v <- vars vec
  norm v =-= CConst d
  return v


prop_hexagon :: V2 N -> V2 N -> V2 N -> V2 N -> V2 N -> V2 N
             -> NonNegative N -> Property
prop_hexagon a b c d e f (NonNegative dist) = t do
  av <- vars a
  bv <- vars b
  cv <- vars c
  dv <- vars d
  ev <- vars e
  fv <- vars f
  distance av bv =-= CConst dist
  distance bv cv =-= CConst dist
  distance cv dv =-= CConst dist
  distance dv ev =-= CConst dist
  distance ev fv =-= CConst dist
  distance fv av =-= CConst dist
  av^._x =-= ev^._x
  bv^._x =-= dv^._x
  fv^._y =-= cv^._y
  distance av dv =-= distance bv ev
  distance bv ev =-= distance cv fv
  distance cv fv =-= distance av dv
  return av


return []
runTests = $quickCheckAll
