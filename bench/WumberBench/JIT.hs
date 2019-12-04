{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}

module WumberBench.JIT where

import Control.DeepSeq (NFData(..), NFData1(..))
import Criterion
import Criterion.Main
import Data.Foldable (foldl')
import Data.IntMap   (IntMap(..))
import Linear.Matrix
import Linear.Metric (distance)
import Linear.V3

import Wumber


-- Test model
model :: FConstraints f R => V3 (Sym f R) -> Sym f R
model = moved_by (V3 0 1.1 0) (bolt 0.5 0.4) `iunion` scs
  where
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
                          cube (BB (V3 minBound minBound 0) (V3 maxBound maxBound 0.5))

    sphere l v = 0.8 - distance v l

    cube (BB (V3 x1 y1 z1) (V3 x2 y2 z2)) (V3 x y z) =
      foldl' lower maxBound [ x - x1, x2 - x, y - y1, y2 - y, z - z1, z2 - z ]


    iunion     f g v = upper (f v) (g v)
    iintersect f g v = lower (f v) (g v)
    inegate    f v   = negate (f v)

    spheres = sphere 0 `iunion` sphere 0.9
    scs     = spheres `iunion` cube (BB (-1.5) (-0.5))
                      `iunion` cube (BB (-1.2) (-0.2))

    moved_by t f v = f (v - t)


model_sym :: Sym () Double
model_sym = model (V3 (var 0) (var 1) (var 2))

model_tg :: ThreadGraph Double
model_tg = thread model_sym


instance NFData a => NFData (ThreadGraph a)
instance NFData a => NFData (Insn a)
instance NFData SymFn1
instance NFData SymFn2


benchmarks = [
  bench "jit/thread" (nf thread model_sym),
  bench "jit/asm"    (nf (assemble_graph (PM 10)) model_tg)
             ]
