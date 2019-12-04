{-# LANGUAGE TemplateHaskell #-}

module WumberTest.ModelAffine where

import Linear.Matrix
import Linear.V3
import Linear.V4
import Test.QuickCheck
import Text.Printf

import Wumber


instance Arbitrary a => Arbitrary (V4 a) where
  arbitrary = V4 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (V3 a) where
  arbitrary = V3 <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (BoundingBox a) where
  arbitrary = BB <$> arbitrary <*> arbitrary


-- Bounding boxes must continue to contain points after transformation, although
-- the transformed version could obviously contain additional points.
prop_bb_transform :: M44 R -> BB3D -> V3 R -> Bool
prop_bb_transform m b v = not (inside b v)
                          || inside (transform (AM3 m) b) (transform (AM3 m) v)


return []
runTests = $quickCheckAll
