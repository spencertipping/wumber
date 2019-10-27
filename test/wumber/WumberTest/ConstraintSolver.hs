{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}

module WumberTest.ConstraintSolver where

import Lens.Micro
import Linear.Metric
import Linear.V2
import Linear.V3
import Test.QuickCheck

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


solve_ε = 1e-6


-- | Linear equations are always solvable as long as the slope isn't too steep
--   for our desired tolerance.
prop_uni_linear :: N -> CVal -> CVal -> CVal -> Property
prop_uni_linear x m b y = _cc_val m /= 0 && abs (_cc_val m) < 100 ==>
  solvable 100 solve_ε do
    v <- var x
    v*m + b =-= y


-- | Quadratic equations are solvable when the initial values are reasonably
--   close to the final values.
prop_uni_quadratic :: NonNegative N -> UnitInterval N -> Bool
prop_uni_quadratic (NonNegative x) (UnitInterval f) = solvable 100 solve_ε do
  v <- var x
  v*v =-= CConst (x*x * 2**f)


prop_v2dist :: V2 N -> UnitInterval N -> Property
prop_v2dist vec (UnitInterval f) = norm vec < 50 ==>
  solvable 100 solve_ε do
    v <- vars vec
    norm v =-= CConst (norm vec * 2**f)


prop_v3dist :: V3 N -> UnitInterval N -> Property
prop_v3dist vec (UnitInterval f) = norm vec < 50 ==>
  solvable 100 solve_ε do
    v <- vars vec
    norm v =-= CConst (norm vec * 2**f)


v1 = V3 3.2872818349610062 0.17974700836563873 (-3.8500449102237173)
f1 = -0.8541325263857837


return []
runTests = $quickCheckAll
