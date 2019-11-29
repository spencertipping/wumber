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


just_tree :: IsoFn (V3 R) -> BB3D -> Int -> Int -> R -> Tree (V3 R)
just_tree f b minn maxn bias = build f b sf bias
  where sf _ n (TM b (v:vs)) _
          | any ((/= signum v) . signum) vs = n < maxn
          | otherwise                       = n < minn


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


model :: FConstraints f R => V3 (Sym f R) -> Sym f R
model = moved_by (V3 0 1.1 0) (bolt 0.5 0.4) `iunion` scs


jit_a_fn :: (V3 (Sym () Double) -> Sym () Double) -> V3 Double -> Double
jit_a_fn m = jit (m (V3 (var 0) (var 1) (var 2))) . to_storable_vector


model_fn  = jit_a_fn model

-- With FFI overhead, model2_fn is ~2x slower than model_fn but produces
-- identical results
model2_fn = jit_a_fn \v -> (model v + model v + model v + model v) / 4


isotree12 = just_tree model_fn (BB (-2) 2) 6  12 0.1
isotree18 = just_tree model_fn (BB (-2) 2) 12 18 0.1
isotree24 = just_tree model_fn (BB (-2) 2) 18 24 0.1


benchmarks = bs
  where info = printf "isotree12: %d; isotree18: %d; isotree24: %d\n"
                      (t_size isotree12) (t_size isotree18) (t_size isotree24)
        bs = [
          bench "model/jit"  (nf model_fn  (V3 0.5 1 2 :: V3 Double))
          -- bench "model/jit2" (nf model2_fn (V3 0.5 1 2 :: V3 Double)),
          -- bench "model/ghc"  (nf model     (V3 0.5 1 2 :: V3 Double)),

          -- bench "tree/jit"  (nf (t_size . just_tree model_fn  (BB (-2) 2) 6 12) 0.1),
          -- bench "tree/jit2" (nf (t_size . just_tree model2_fn (BB (-2) 2) 6 12) 0.1),
          -- bench "tree/hs"   (nf (t_size . just_tree model     (BB (-2) 2) 6 12) 0.1),

          -- bench "contour/trace/12" (nf (length . trace_lines) isotree12),
          -- bench "contour/trace/18" (nf (length . trace_lines) isotree18),
          -- bench "contour/trace/24" (nf (length . trace_lines) isotree24),

          -- bench "contour12/jit"  (nf (length . iso_contour model_fn  (BB (-2) 2) 6 12) 0.1)
          -- bench "contour12/jit2" (nf (length . iso_contour model2_fn (BB (-2) 2) 6 12) 0.1),
          -- bench "contour12/hs"   (nf (length . iso_contour model     (BB (-2) 2) 6 12) 0.1)
          ]
