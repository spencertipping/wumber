{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Main where

import Control.Monad
import Debug.Trace
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


-- NOTE: can't be Float; the model never converges
type OurNumber = Double

data Implicit = ISphere !OurNumber !(V3 OurNumber)
              | Intersect [Implicit]
              | Union     [Implicit]
              | Negate    Implicit

for = flip map
fi  = fromIntegral


eval_sphere (ISphere r c) v = r - distance c v

eval_sphere_handcoded (ISphere r (V3 cx cy cz)) (V3 x y z) =
  r - sqrt ( dx*dx + dy*dy + dz*dz )
  where dx = cx - x
        dy = cy - y
        dz = cz - z


-- NOTE: eval is 16x slower than eval_handcoded without strictness annotations;
-- with annotations, the two functions have the same performance within 5%.
--
-- Stream fusion doesn't happen between foldl1 and map. eval union with
-- foldl1 max $ map ... is 40ns (100%) slower than the manually fused version.
eval :: Implicit -> V3 OurNumber -> OurNumber
eval (ISphere r !l) !v  = r - distance v l
eval (Union !xs) !v     = foldl (\d x -> d `max` eval x v) (-1/0) xs
--eval (Intersect !xs) !v = foldl1 min $ map (\m -> eval m v) xs
--eval (Negate !x) !v     = negate $ eval x v


eval_sphere_mono :: Implicit -> V3 OurNumber -> OurNumber
--eval_sphere_mono = eval         -- 30% slower
eval_sphere_mono = eval_sphere

eval_union_mono :: Implicit -> V3 OurNumber -> OurNumber
eval_union_mono (Union !xs) !v =
  foldl (\d x -> d `max` eval_sphere_mono x v) (-1/0) xs


ε = 1e-8

-- | Returns the location and gradient of the surface intersection.
solve :: (V3 OurNumber -> OurNumber) -> V3 OurNumber -> (V3 OurNumber, V3 OurNumber)
solve f v | abs d <= ε = (v, g)
          | otherwise  = solve f (v ^-^ g ^* abs d / 2)
  where !d = f v
        !g = V3 (f (v ^+^ (V3 ε 0 0)) - d)
                (f (v ^+^ (V3 0 ε 0)) - d)
                (f (v ^+^ (V3 0 0 ε)) - d) ^/ ε


-- NOTE: eval_model_handcoded is faster with these variables than it is with
-- (spheres !! 0) index expressions. This seems counterintuitive to me;
-- shouldn't those be compute-once?
s1 = ISphere 5 1
s2 = ISphere 4 2
s3 = ISphere 3 3

spheres = [s1, s2, s3]
eval_model v = eval (Union spheres) v
eval_model_mono v = eval_union_mono (Union spheres) v

eval_model_handcoded v =
  v1 `max` v2 `max` v3
  where v1 = eval_sphere_handcoded s1 v
        v2 = eval_sphere_handcoded s2 v
        v3 = eval_sphere_handcoded s3 v


main = defaultMain
  [
    bench "solve union" $ nf (solve eval_model) 0.1,
    bench "solve union mono" $ nf (solve eval_model_mono) 0.1,
    bench "solve union handcoded"
      $ nf (solve eval_model_handcoded) 0.1
  ]
