module Main where

import Criterion
import Criterion.Main

import Linear.Metric
import Linear.V3


type HandcodedIsoFn = (Double, Double, Double) -> Double
type VectorIsoFn = V3 Double -> Double


handcoded_sphere :: Double -> Double -> Double -> Double -> HandcodedIsoFn
handcoded_sphere r x y z (vx, vy, vz) = r - sqrt (dx*dx + dy*dy + dz*dz)
  where dx = vx - x
        dy = vy - y
        dz = vz - z


vector_sphere :: Double -> V3 Double -> VectorIsoFn
vector_sphere r l v = r - distance l v


main = defaultMain
  [
    bench "handcoded sphere" (nf (handcoded_sphere 2     0 1 2)   (1, 2, 3)),
    bench "vector sphere"    (nf (vector_sphere    2 (V3 0 1 2)) (V3 1 2 3))
  ]
