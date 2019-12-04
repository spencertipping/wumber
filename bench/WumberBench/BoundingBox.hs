{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module WumberBench.BoundingBox (benchmarks) where

import Criterion
import Criterion.Main
import Linear.V3
import System.IO.Unsafe (unsafePerformIO)
import Test.QuickCheck

import Wumber.BoundingBox


bb_intersects :: BB3D -> BB3D -> Bool
bb_intersects = intersects

bb_collapsed_dims :: BB3D -> BB3D -> Int
bb_collapsed_dims = collapsed_dimensions


hc_intersects :: BB3D -> BB3D -> Bool
hc_intersects (BB (V3 x1 y1 z1) (V3 x2 y2 z2)) (BB (V3 x3 y3 z3) (V3 x4 y4 z4)) =
  max x1 x3 <= min x2 x4 && max y1 y3 <= min y2 y4 && max z1 z3 <= min z2 z4


hc_collapsed_dims :: BB3D -> BB3D -> Int
hc_collapsed_dims (BB (V3 x1 y1 z1) (V3 x2 y2 z2))
                  (BB (V3 x3 y3 z3) (V3 x4 y4 z4)) =
  (if max x1 x3 >= min x2 x4 then 1 else 0) +
  (if max y1 y3 >= min y2 y4 then 1 else 0) +
  (if max z1 z3 >= min z2 z4 then 1 else 0)


instance Arbitrary (V3 Double) where
  arbitrary = V3 <$> arbitrary <*> arbitrary <*> arbitrary


instance Arbitrary BB3D where
  arbitrary = BB <$> arbitrary <*> arbitrary


verify = do
  quickCheck \a b -> bb_intersects     a b == hc_intersects     a b
  quickCheck \a b -> bb_collapsed_dims a b == hc_collapsed_dims a b


benchmarks = unsafePerformIO do
  verify
  return [
    bench "bb/gen-intersects" (nf (bb_intersects (BB 1 2)) (BB 3 4)),
    bench "bb/hc-intersects"  (nf (hc_intersects (BB 1 2)) (BB 3 4)),

    bench "bb/gen-cdims" (nf (bb_collapsed_dims (BB 1 2)) (BB 3 4)),
    bench "bb/hc-cdims"  (nf (hc_collapsed_dims (BB 1 2)) (BB 3 4))
    ]
