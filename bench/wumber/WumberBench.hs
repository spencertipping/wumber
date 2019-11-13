{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Control.Applicative
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
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.ByteString as BS

import Wumber

import qualified WumberBench.HandcodedFns as HF
import qualified WumberBench.HintLoad     as HL


test_hint :: IO (Either InterpreterError (V2 Double))
test_hint = runInterpreter do
  setImports ["Prelude", "Linear.V2"]
  interpret "V2 1 2" (as :: V2 Double)


eitherError (Left a)  = error (show a)
eitherError (Right a) = a


load_linear :: IO (V3 Double)
load_linear = eitherError <$> runInterpreter do
  setImports ["Prelude", "Linear.Metric", "Linear.V2", "Linear.V3", "Linear.Vector"]
  interpret "V3 1 2 3" (as :: V3 Double)


load_wumbersym :: IO Double
load_wumbersym = eitherError <$> runInterpreter do
  setImports ["Prelude", "Wumber.Symbolic"]
  interpret "eval id $ N 4 + N 5" (as :: Double)


load_wumbersym_nop :: IO Double
load_wumbersym_nop = eitherError <$> runInterpreter do
  setImports ["Prelude", "Wumber.Symbolic"]
  interpret "4 + 5" (as :: Double)

load_wumber_all :: IO Double
load_wumber_all = eitherError <$> runInterpreter do
  setImports ["Prelude", "Wumber"]
  interpret "eval id $ N 4 + N 5" (as :: Double)


hint_baseline :: IO Double
hint_baseline = eitherError <$> runInterpreter do
  setImports ["Prelude"]
  interpret "4 + 5" (as :: Double)


-- Test model
threads p d v@(V3 x y z) = p ρ z'
  where θ  = atan2 x y
        ρ  = sqrt (x**2 + y**2)
        z' = ((z * d + θ/τ) % 1 + 1) % 1

t45 od ρ z = od - sin (τ/6) * ρ + abs (z - 0.5)


x_lt l (V3 x _ _) = l - x
z_lt l (V3 _ _ z) = l - z

hex_cap r v = foldl' lower maxBound
  $ map (\θ -> x_lt r (v *! rotate_z_m (from_floating θ))) [0, 60 .. 300]


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

-- OK this constraint set is just silly.
model :: (ClosedComparable a, FromFloating a a, Fractional a, Floating a, FromFloating Integer a, Bounded a, RealFloat a, Mod a) => V3 a -> a
model = bolt 0.5 0.4 `iunion` scs


jit_a_fn :: (V3 (Sym Double) -> Sym Double) -> V3 Double -> Double
jit_a_fn m = unsafePerformIO do
  fp <- compile dblfn $ assemble_ssa (linearize (m (V3 (Arg 0) (Arg 1) (Arg 2))))
  return $ unsafePerformIO . flip unsafeWith fp . to_storable_vector


model_fn  = jit_a_fn model

-- With FFI overhead, model2_fn is 2x slower than model_fn but produces
-- identical results
model2_fn = jit_a_fn \v -> (model v + model v + model v + model v) / 4


main = defaultMain $
  HF.benchmarks ++
  HL.benchmarks ++
  [
    -- bench "HC sphere" (nf handcoded_const_sphere                    (1, 2, 3)),
    -- bench "HV sphere" (nf (handcoded_vector_sphere 2 (V3 0.5 1 2)) (V3 1 2 3)),

    {-
    bench "H  sphere" (nf (handcoded_sphere        2     0.5 1 2)   (1, 2, 3)),
    bench "V  sphere" (nf (vector_sphere           2 (V3 0.5 1 2)) (V3 1 2 3)),

    unsafePerformIO $ unsafeWith (fromList [1::Double, 2, 3]) \p -> do
      return $ bench "J  sphere" (nf jit_sphere_fn p),

    bench "J  limit"  (nf jit_limit_fn  nullPtr),
    bench "J  limit2" (nf jit_limit2_fn nullPtr),
    bench "J  limit3" (nf jit_limit3_fn nullPtr),
    -}

    bench "jit model"  (nf model_fn  (V3 0.5 1 2 :: V3 Double)),
    bench "jit model2" (nf model2_fn (V3 0.5 1 2 :: V3 Double)),
    bench "hs  model"  (nf model     (V3 0.5 1 2 :: V3 Double)),

    bench "contour jit"  (nf (length . iso_contour model_fn  (BB (-2) 2) 6 12) 0.1),
    bench "contour jit2" (nf (length . iso_contour model2_fn (BB (-2) 2) 6 12) 0.1),
    bench "contour hs"   (nf (length . iso_contour model     (BB (-2) 2) 6 12) 0.1)

    {-
    bench "HV cube"   (nf (handcoded_vector_cube (V3 1 2 3) (V3 4 5 6)) (V3 7 8 9)),
    bench "FV cube"   (nf (fold_vector_cube      (V3 1 2 3) (V3 4 5 6)) (V3 7 8 9)),
    bench "AV cube"   (nf (app_vector_cube       (V3 1 2 3) (V3 4 5 6)) (V3 7 8 9)),
    -}

    {-
    bench "hint V3"            (nfIO load_linear),
    bench "hint baseline"      (nfIO hint_baseline),
    bench "hint wumbersym"     (nfIO load_wumbersym),
    bench "hint wumbersym_nop" (nfIO load_wumbersym_nop),
    bench "hint wumber_all"    (nfIO load_wumber_all)
    -}
  ]
