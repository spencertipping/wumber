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
import Wumber.GeometricConstraints


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
--   GSL specifies tolerance in terms of /input/ distance, not output cost
--   function distance from the optimum -- so I'm verifying by assuming the
--   resulting cost will be at most √δ. There's no mathematical rigor to this
--   other than saying it's half as precise in log-terms.

solvable :: (Rewritable a b, Show b) => N -> Int -> Constrained a -> Property
solvable δ n m = counterexample (show (xs, v, a)) $ v <= ε
  where ε           = sqrt δ
        (a, xs, cs) = solve_full δ n m
        v           = eval_constraints cs xs


solve_δ    = 1e-6
iterations = 10000

t = solvable solve_δ iterations


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
             -> Positive N -> Property
prop_hexagon a b c d e f (Positive dist) = t do
  av <- vars a
  bv <- vars b
  cv <- vars c
  dv <- vars d
  ev <- vars e
  fv <- vars f

  all_equal [CConst dist,
             distance av bv,
             distance bv cv,
             distance cv dv,
             distance dv ev,
             distance ev fv,
             distance fv av]

  -- TODO: these fail to converge if all points have equal starting values (and
  -- possibly in other cases).
  {-
  all_equal [cos $ CConst (τ / 6),
             inner_angle_cos av bv cv,
             inner_angle_cos bv cv dv,
             inner_angle_cos cv dv ev,
             inner_angle_cos dv ev fv,
             inner_angle_cos ev fv av,
             inner_angle_cos fv av bv]
  -}

  aligned _x [av, ev]
  aligned _x [bv, dv]

  -- TODO: why do these constraints cause the test case to fail to converge?
  --aligned _y   [av, bv]
  --aligned _y [fv,     cv]
  --aligned _y   [ev, dv]

  all_equal [distance av dv,
             distance bv ev,
             distance cv fv]

  return av


return []
runTests = $quickCheckAll
