{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module WumberBench.Contour where

import Control.Applicative
import Criterion
import Criterion.Main
import Data.Either
import Data.Foldable
import Data.Vector.Storable (Vector, fromList, unsafeWith)
import Debug.Trace (trace)
import Foreign.Ptr (castPtrToFunPtr, nullPtr)
import Linear.Matrix ((*!))
import Linear.Metric
import Linear.V2
import Linear.V3
import Numeric (showHex)
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf (printf)

import qualified Data.ByteString as BS

import Wumber
import Wumber.DualContour
import Wumber.SymbolicDerivative
import Wumber.SymbolicJIT


-- Test model
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


-- Isofunctions for testing
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


model :: V3 (Sym () R) -> Sym () R
model = moved_by (V3 0 1.1 0) (bolt 0.5 0.4) `iunion` scs


tiny_model :: V3 (Sym () R) -> Sym () R
tiny_model (V3 x y z) = x + y + z + 1


model_frep = FRep (SymV (model v3)) (BB (-2) 2)

inline_compute f f' bb = toList $ iso_contour f f' bb 6 18 0.1

pre_jit :: Sym () R -> (IsoFn (V3 R), IsoGradient (V3 R))
pre_jit s = fv `seq` (f, f')
  where fv   = fmap jit (vector_derivative (SymV s)) :: V3 (V3 R -> R)
        f    = jit s
        f' v = fv <*> pure v


benchmarks = [
  bench "model/jittiny" (nf (jit (tiny_model v3)) (V3 0.5 1 2 :: V3 Double)),
  bench "model/jit"     (nf (jit (model v3))      (V3 0.5 1 2 :: V3 Double)),
  bench "model/frep"    (nf (unSketch . compute :: FRep V3 () -> [(V3 R, V3 R)]) model_frep),
  bench "model/frep-pre" (nf (inline_compute f f') (BB (-2) 2))
  ]
  where (f, f') = pre_jit (model v3)
