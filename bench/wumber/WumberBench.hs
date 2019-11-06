module Main where

import Control.Applicative
import Criterion
import Criterion.Main
import Data.Foldable

import Linear.Metric
import Linear.V3


type HandcodedIsoFn = (Double, Double, Double) -> Double
type VectorIsoFn = V3 Double -> Double


handcoded_sphere :: Double -> Double -> Double -> Double -> HandcodedIsoFn
handcoded_sphere r x y z (vx, vy, vz) = r - sqrt (dx*dx + dy*dy + dz*dz)
  where dx = vx - x
        dy = vy - y
        dz = vz - z


handcoded_const_sphere :: HandcodedIsoFn
handcoded_const_sphere (x, y, z) = 2 - sqrt (dx*dx + dy*dy + dz*dz)
  where dx = x - 0.5
        dy = y - 1
        dz = z - 2


vector_sphere :: Double -> V3 Double -> VectorIsoFn
vector_sphere r l v = r - distance l v


handcoded_vector_sphere :: Double -> V3 Double -> VectorIsoFn
handcoded_vector_sphere r (V3 x y z) (V3 vx vy vz) = r - sqrt (dx*dx + dy*dy + dz*dz)
  where dx = vx - x
        dy = vy - y
        dz = vz - z


handcoded_vector_cube :: V3 Double -> V3 Double -> VectorIsoFn
handcoded_vector_cube (V3 x1 y1 z1) (V3 x2 y2 z2) (V3 x y z) =
  min (min (x - x1) (x2 - x))
      (min (min (y - y1) (y2 - y))
           (min (z - z1) (z2 - z)))


fold_vector_cube :: V3 Double -> V3 Double -> VectorIsoFn
fold_vector_cube (V3 x1 y1 z1) (V3 x2 y2 z2) (V3 x y z) =
  foldl' min (1/0) [ x - x1, x2 - x, y - y1, y2 - y, z - z1, z2 - z ]


app_vector_cube :: (Applicative f, Foldable f)
                => f Double -> f Double -> f Double -> Double
app_vector_cube a b x = foldl' min (1/0) $ liftA2 min lower upper
  where lower = liftA2 (-) x a
        upper = liftA2 (-) b x

-- {-# SPECIALIZE app_vector_cube :: V3 Double -> V3 Double -> V3 Double -> Double #-}


main = defaultMain
  [
    bench "HC sphere" (nf handcoded_const_sphere                    (1, 2, 3)),
    bench "H  sphere" (nf (handcoded_sphere        2     0.5 1 2)   (1, 2, 3)),
    bench "HV sphere" (nf (handcoded_vector_sphere 2 (V3 0.5 1 2)) (V3 1 2 3)),
    bench "V  sphere" (nf (vector_sphere           2 (V3 0.5 1 2)) (V3 1 2 3)),

    bench "HV cube"   (nf (handcoded_vector_cube (V3 1 2 3) (V3 4 5 6)) (V3 7 8 9)),
    bench "FV cube"   (nf (fold_vector_cube      (V3 1 2 3) (V3 4 5 6)) (V3 7 8 9)),
    bench "AV cube"   (nf (app_vector_cube       (V3 1 2 3) (V3 4 5 6)) (V3 7 8 9))
  ]
