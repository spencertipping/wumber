{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Main where

import Control.Monad
import GHC.Float
import Graphics.Gloss
import Linear.Metric
import Linear.V2
import Linear.V3
import Linear.Vector
import System.Clock
import System.IO
import Text.Printf

import Criterion.Main


data Implicit = ISphere !Double !(V3 Double)
              | Intersect [Implicit]
              | Union     [Implicit]
              | Negate    Implicit

for = flip map
fi  = fromIntegral
f2d = float2Double


-- WAT: this is 16x faster than eval???
eval_sphere_handcoded (V3 x y z) (ISphere r (V3 cx cy cz)) =
  r - sqrt ( dx*dx + dy*dy + dz*dz )
  where dx = cx - x
        dy = cy - y
        dz = cz - z


eval :: V3 Double -> Implicit -> Double
eval v (ISphere r l)  = r - distance v l
eval v (Intersect xs) = foldl1 min $ map (eval v) xs
eval v (Union xs)     = foldl1 max $ map (eval v) xs
eval v (Negate x)     = negate $ eval v x


ε = 1e-8

-- | Returns the location and gradient of the surface intersection.
solve :: V3 Double -> Implicit -> (V3 Double, V3 Double)
solve v i
  | abs d <= ε = (v, V3 gx gy gz)
  | otherwise  = solve (v ^-^ V3 gx gy gz ^/ d) i
  where d  = eval v i
        gx = eval (v ^+^ (V3 ε 0 0)) i - d
        gy = eval (v ^+^ (V3 0 ε 0)) i - d
        gz = eval (v ^+^ (V3 0 0 ε)) i - d


main = defaultMain
  [
    bench "eval sphere" $ nf (eval 0.1) (ISphere 5 0),
    bench "eval sphere handcoded" $ nf (eval_sphere_handcoded 0.1) (ISphere 5 0)
  ]
