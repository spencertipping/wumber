{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

import qualified Data.Vector.Storable as VS

import Debug.Trace

import Wumber.Constraint
import Wumber.ConstraintSolver
import Wumber.GeometricConstraints
import Wumber.Numeric
import Wumber.Symbolic


newtype UnitInterval a = UnitInterval a deriving Show
instance Arbitrary (UnitInterval R) where
  arbitrary = UnitInterval <$> choose (-1, 1)


instance Arbitrary (CVal ()) where arbitrary = val <$> arbitrary

instance Arbitrary (V2 R) where
  arbitrary = V2 <$> arbitrary <*> arbitrary

instance Arbitrary (V3 R) where
  arbitrary = V3 <$> arbitrary <*> arbitrary <*> arbitrary


-- | A system is solvable iff it converges to error below the epsilon and hasn't
--   exhausted its iteration count.
--
--   GSL specifies tolerance in terms of /input/ distance, not output cost
--   function distance from the optimum -- so I'm verifying by assuming the
--   resulting cost will be at most √δ. There's no mathematical rigor to this
--   other than saying it's half as precise in log-terms.

solvable :: (FConstraints f R, Rewritable f a b, Show b)
         => R -> Int -> Constrained f a -> Property
solvable δ n m = counterexample (show (xs, v, a)) $ v <= ε
  where ε           = sqrt δ
        (a, xs, cs) = solve_full δ n m
        v           = eval (xs VS.!) (constraint_cost cs)


solve_δ    = 1e-6
iterations = 10000

t = solvable solve_δ iterations


prop_uni_linear :: R -> NonZero R -> CVal () -> CVal () -> Property
prop_uni_linear x (NonZero m) b y = t do
  v <- cvar x
  v * val m + b =-= y
  return [v]


prop_uni_quadratic :: NonNegative R -> NonNegative R -> Property
prop_uni_quadratic (NonNegative x) (NonNegative y) = t do
  v :: CVal () <- cvar x
  v*v =-= val y
  return [v]


prop_v2dist :: V2 R -> NonNegative R -> Property
prop_v2dist vec (NonNegative d) = t do
  v :: V2 (CVal ()) <- cvars vec
  norm v =-= val d
  return v


prop_v2joint_lt :: V2 R -> NonNegative R -> Property
prop_v2joint_lt vec (NonNegative d) = t do
  v :: V2 (CVal ()) <- cvars vec
  v^._x  <-= v^._y
  norm v =-= val d
  return v

prop_v2joint_eq :: V2 R -> NonNegative R -> Property
prop_v2joint_eq vec (NonNegative d) = t do
  v :: V2 (CVal ()) <- cvars vec
  v^._x  =-= v^._y
  norm v =-= val d
  return v


prop_v3dist :: V3 R -> NonNegative R -> Property
prop_v3dist vec (NonNegative d) = t do
  v :: V3 (CVal ()) <- cvars vec
  norm v =-= val d
  return v


-- TODO
-- This test isn't as robust as I want, nor is it very realistic. It fails every
-- ~10k runs or so.
--
-- The problem is that the constraint solver depends on the initial values of
-- the variables you give it. This is fine in practice; you can easily fix
-- those, and it isn't very sensitive. But certain inputs will result in no
-- solution. This test manages to find those.

prop_hexagon :: V2 R -> V2 R -> V2 R -> V2 R -> V2 R -> V2 R
             -> Positive R -> Property
prop_hexagon a b c d e f (Positive dist) = t do
  av :: V2 (CVal ()) <- cvars a
  bv :: V2 (CVal ()) <- cvars b
  cv :: V2 (CVal ()) <- cvars c
  dv :: V2 (CVal ()) <- cvars d
  ev :: V2 (CVal ()) <- cvars e
  fv :: V2 (CVal ()) <- cvars f

  all_equal [val dist,
             distance av bv,
             distance bv cv,
             distance cv dv,
             distance dv ev,
             distance ev fv,
             distance fv av]

  -- TODO: these fail to converge if all points have equal starting values (and
  -- possibly in other cases).
  {-
  all_equal [cos $ N (τ / 6),
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
