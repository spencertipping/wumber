{-# LANGUAGE BlockArguments #-}

module Main where

import Control.Applicative
import Criterion
import Criterion.Main
import Data.Either
import Data.Foldable
import Data.Vector.Storable (Vector, fromList, unsafeWith)
import Foreign.Ptr (castPtrToFunPtr)
import Language.Haskell.Interpreter
import Linear.Metric
import Linear.V2
import Linear.V3
import System.IO.Unsafe (unsafePerformIO)

import Wumber.AMD64Asm
import Wumber.DualContour
import Wumber.JIT
import Wumber.JITIR
import Wumber.Symbolic


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


type HandcodedIsoFn = (Double, Double, Double) -> Double
type VectorIsoFn = V3 Double -> Double
type JitIsoFn = Vector Double -> Double


jit_sphere_vfn r v l = r - distance v l

jit_sphere_sym  = jit_sphere_vfn 2 (V3 0.5 1 2) (V3 (Arg 0) (Arg 1) (Arg 2))
jit_sphere_code = assemble_ssa $ linearize jit_sphere_sym
jit_sphere_fn   = unsafePerformIO . f
  where f = unsafePerformIO $ dblfn <$> castPtrToFunPtr <$> compile jit_sphere_code


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


handcoded_vector_cube :: V3 Double -> V3 Double -> VectorIsoFn
handcoded_vector_cube (V3 x1 y1 z1) (V3 x2 y2 z2) (V3 x y z) =
  min (min (x - x1) (x2 - x))
      (min (min (y - y1) (y2 - y))
           (min (z - z1) (z2 - z)))


fold_vector_cube :: V3 Double -> V3 Double -> VectorIsoFn
fold_vector_cube (V3 x1 y1 z1) (V3 x2 y2 z2) (V3 x y z) =
  foldl' min (1/0) [ x - x1, x2 - x, y - y1, y2 - y, z - z1, z2 - z ]


app_vector_cube :: (Applicative f, Foldable f)
                => f Double -> f Double -> f Double -> Double
app_vector_cube a b x = foldl' min (1/0) $ liftA2 min lower upper
  where lower = liftA2 (-) x a
        upper = liftA2 (-) b x

-- Makes no difference:
-- {-# SPECIALIZE app_vector_cube :: V3 Double -> V3 Double -> V3 Double -> Double #-}


main = defaultMain
  [
    -- bench "HC sphere" (nf handcoded_const_sphere                    (1, 2, 3)),
    -- bench "HV sphere" (nf (handcoded_vector_sphere 2 (V3 0.5 1 2)) (V3 1 2 3)),

    bench "H  sphere" (nf (handcoded_sphere        2     0.5 1 2)   (1, 2, 3)),
    bench "V  sphere" (nf (vector_sphere           2 (V3 0.5 1 2)) (V3 1 2 3)),
    unsafePerformIO $ unsafeWith (fromList [1::Double, 2, 3]) \p -> do
      return $ bench "J  sphere" (nf jit_sphere_fn p)

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
