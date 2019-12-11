{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections #-}

{-# OPTIONS -fobject-code #-}

module Iso (example) where

import Data.Foldable
import Linear.Matrix ((*!))
import Linear.Metric
import Linear.V3
import Linear.V4
import Linear.Vector

import Wumber


example :: FRep V3 MathFn
example = FRep (SymMathV $ model $ screw 0 v3) (BB (-2 :: V3 R) 2)


for  = flip map
cfor = flip concatMap


screw :: SymMath MathFn R -> V3 (SymMath MathFn R) -> V3 (SymMath MathFn R)
screw dθ v@(V3 x y z) = v *! rotate_z_m (dθ * z)


-- Hex-cap machine bolt, screw threads and all

-- First let's model the threads: a triangular section screw-extruded along an
-- axis. I'll space the threads one unit apart and extrude along the Z axis.
--
-- The profile function should be an iso over ρ and z.

threads p d v@(V3 x y z) = p ρ z'
  where θ  = atan2 x y
        ρ  = sqrt (x**2 + y**2)
        z' = ((z * d + θ/τ) `mod` 1 + 1) `mod` 1

t45 od ρ z = od - sin (τ/6) * ρ + abs (z - 0.5)


x_lt l (V3 x _ _) = l - x
z_lt l (V3 _ _ z) = l - z

hex_cap r v = foldl' lower maxBound
  $ map (\θ -> x_lt r (v *! rotate_z_m (val θ))) [0, 60 .. 300]


bolt od ts = thread_part `iunion` head_part
  where thread_part = (threads (t45 od) 2 . (/ ts)) `iintersect` z_lt 0
        head_part   = hex_cap od `iintersect`
                      cube (BB (V3 minBound minBound 0) (V3 maxBound maxBound 0.4))


-- Isofunctions for testing
sphere l v = 0.9 - distance v l

cube (BB (V3 x1 y1 z1) (V3 x2 y2 z2)) (V3 x y z) =
  foldl' lower maxBound [ x - x1, x2 - x, y - y1, y2 - y, z - z1, z2 - z ]


iunion     f g v = upper (f v) (g v)
iintersect f g v = lower (f v) (g v)
inegate    f v   = negate (f v)


moved_by t f v = f (v - t)


model = moved_by (V3 0.3 1.3 (-0.4)) (bolt 0.5 0.4) `iunion` scs

spheres = sphere 0 `iunion` sphere 0.9 `iunion` sphere (V3 (-0.4) (-0.4) 1)
scs     = spheres `iunion` cube (BB (-1.5) (-0.5))
                  `iunion` cube (BB (-1.2) (-0.2))
