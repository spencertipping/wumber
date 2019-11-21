{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{-# OPTIONS -fobject-code #-}

module Examples.Iso where

import Control.Monad
import Control.Monad.Identity
import Control.Monad.RWS.Strict
import Control.Concurrent.MVar
import Criterion
import Criterion.Main
import Criterion.Main.Options
import Data.Foldable
import Data.Vector.Storable (fromList, unsafeWith)
import Debug.Trace
import Graphics.Gloss
import Linear.Matrix ((*!))
import Linear.Metric
import Linear.V3
import Linear.V4
import Linear.Vector
import System.IO (stderr)
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf

import Wumber


for  = flip map
cfor = flip concatMap


screw :: Double -> V3 Double -> V3 Double
screw dθ v@(V3 x y z) = v *! rotate_z_m (dθ * z)


-- Hex-cap machine bolt, screw threads and all

-- First let's model the threads: a triangular section screw-extruded along an
-- axis. I'll space the threads one unit apart and extrude along the Z axis.
--
-- The profile function should be an iso over ρ and z.

threads p d v@(V3 x y z) = p ρ z'
  where θ  = Fn2 Atan2 x y
        ρ  = sqrt (x**2 + y**2)
        z' = ((z * d + θ/τ) % 1 + 1) % 1

t45 od ρ z = od - sin (τ/6) * ρ + abs (z - 0.5)


x_lt l (V3 x _ _) = l - x
z_lt l (V3 _ _ z) = l - z

hex_cap r v = foldl' lower maxBound
  $ map (\θ -> x_lt r (v *! rotate_z_m (N θ))) [0, 60 .. 300]


bolt od ts = thread_part `iunion` head_part
  where thread_part = (threads (t45 od) 2 . (/ ts)) `iintersect` z_lt 0
        head_part   = hex_cap od `iintersect`
                      cube (BB (V3 minBound minBound 0) (V3 maxBound maxBound 0.5))


-- Isofunctions for testing
sphere l v = 0.8 - distance v l

cube (BB (V3 x1 y1 z1) (V3 x2 y2 z2)) (V3 x y z) =
  foldl' lower maxBound [ x - x1, x2 - x, y - y1, y2 - y, z - z1, z2 - z ]


iunion     f g v = upper (f v) (g v)
iintersect f g v = lower (f v) (g v)
inegate    f v   = negate (f v)


moved_by t f v = f (v - t)


model = moved_by (V3 0 1.1 0) (bolt 0.5 0.4) `iunion` scs
--model v = threads (t45 0.5) (v * 3) -- scs v -- `upper` cubearray (v / 2) -- + cubes v * (-0.3)

spheres = sphere 0 `iunion` sphere 0.9
scs     = spheres `iunion` cube (BB (-1.5) (-0.5))
                  `iunion` cube (BB (-1.2) (-0.2))

cubes   = cube (BB (-1) 1)


main :: Wumber ()
main = do
  zoom 0.005
  tell [model (V3 (Arg 0) (Arg 1) (Arg 2))]
