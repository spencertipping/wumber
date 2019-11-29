{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module WumberBench.HandcodedFns (benchmarks) where


import Control.Applicative
import Control.Concurrent
import Criterion
import Criterion.Main
import Data.Either
import Data.Foldable
import Data.Vector.Storable (Vector, fromList, unsafeWith)
import Foreign.Ptr (castPtrToFunPtr, nullPtr)
import Language.Haskell.Interpreter
import Linear.Matrix ((*!))
import Linear.Metric
import Linear.V2
import Linear.V3
import Numeric (showHex)
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import System.Process

import qualified Data.ByteString as BS

import Wumber


ndisasm :: BS.ByteString -> IO BS.ByteString
ndisasm code = do
  (Just i, Just o, _, p) <- createProcess
                            (proc "ndisasm" ["-b64", "-"]) { std_in  = CreatePipe,
                                                             std_out = CreatePipe }
  forkIO do BS.hPut i code
            hClose i
  !b <- BS.hGetContents o
  waitForProcess p
  return b


jit_a_fn :: (V3 (Sym () Double) -> Sym () Double) -> V3 Double -> Double
jit_a_fn m = jit (m (V3 (var 0) (var 1) (var 2))) . to_storable_vector


type HandcodedIsoFn = (Double, Double, Double) -> Double
type VectorIsoFn = V3 Double -> Double


jit_sphere_vfn r v l = r - distance v l

jit_sphere_fn :: VectorIsoFn
jit_sphere_fn = jit_a_fn (jit_sphere_vfn 2 (V3 0.5 1 2))


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


handcoded_vector_cube (V3 x1 y1 z1) (V3 x2 y2 z2) (V3 x y z) =
  lower (lower (x - x1) (x2 - x))
        (lower (lower (y - y1) (y2 - y))
               (lower (z - z1) (z2 - z)))


fold_vector_cube (V3 x1 y1 z1) (V3 x2 y2 z2) (V3 x y z) =
  foldl' lower maxBound [ x - x1, x2 - x, y - y1, y2 - y, z - z1, z2 - z ]


app_vector_cube a b x = foldl' min maxBound $ liftA2 min lower upper
  where lower = liftA2 (-) x a
        upper = liftA2 (-) b x

-- Makes no difference:
-- {-# SPECIALIZE app_vector_cube :: V3 Double -> V3 Double -> V3 Double -> Double #-}


jit_cube = jit_a_fn (fold_vector_cube (V3 1 2 3) (V3 4 5 6))


benchmarks = [
  bench "sphere/hc"  (nf handcoded_const_sphere                    (1, 2, 3)),
  bench "sphere/hv"  (nf (handcoded_vector_sphere 2 (V3 0.5 1 2)) (V3 1 2 3)),

  bench "sphere/h"   (nf (handcoded_sphere        2     0.5 1 2)   (1, 2, 3)),
  bench "sphere/v"   (nf (vector_sphere           2 (V3 0.5 1 2)) (V3 1 2 3)),
  bench "sphere/jit" (nf jit_sphere_fn (V3 1 2 3)),

  bench "cube/hv"    (nf (handcoded_vector_cube (V3 1 2 3 :: V3 Double) (V3 4 5 6)) (V3 7 8 9)),
  bench "cube/fold"  (nf (fold_vector_cube      (V3 1 2 3 :: V3 Double) (V3 4 5 6)) (V3 7 8 9)),
  bench "cube/app"   (nf (app_vector_cube       (V3 1 2 3 :: V3 Double) (V3 4 5 6)) (V3 7 8 9)),
  bench "cube/jit"   (nf jit_cube (V3 7 8 9))
             ]
