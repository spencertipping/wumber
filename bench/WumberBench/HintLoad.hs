{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module WumberBench.HintLoad (benchmarks) where

import Control.Applicative
import Criterion
import Criterion.Main
import Data.Either
import Data.Foldable
import Data.Vector.Storable (Vector, fromList, unsafeWith)
import Debug.Trace (trace)
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


test_hint :: IO (Either InterpreterError (V2 Double))
test_hint = runInterpreter do
  setImports ["Prelude", "Linear.V2"]
  interpret "V2 1 2" (as :: V2 Double)


eitherError (Left a)  = error (show a)
eitherError (Right a) = a


-- TODO
-- Hint can't load Linear.V3 from 'stack bench' ... not sure what's going on.

load_linear :: IO (V3 Double)
load_linear = eitherError <$> runInterpreter do
  setImports ["Prelude", "Linear.Metric", "Linear.V2", "Linear.V3", "Linear.Vector"]
  interpret "V3 1 2 3" (as :: V3 Double)


load_wumbersym :: IO Double
load_wumbersym = eitherError <$> runInterpreter do
  setImports ["Prelude", "Wumber.Symbolic"]
  interpret "eval (const 0) $ N 4 + N 5" (as :: Double)


load_wumbersym_nop :: IO Double
load_wumbersym_nop = eitherError <$> runInterpreter do
  setImports ["Prelude", "Wumber.Symbolic"]
  interpret "4 + 5" (as :: Double)

load_wumber_all :: IO Double
load_wumber_all = eitherError <$> runInterpreter do
  setImports ["Prelude", "Wumber"]
  interpret "eval (const 0) $ N 4 + N 5" (as :: Double)


hint_baseline :: IO Double
hint_baseline = eitherError <$> runInterpreter do
  setImports ["Prelude"]
  interpret "4 + 5" (as :: Double)


benchmarks = trace "skipping hint benchmarks (TODO: fix)" [
  {-
  bench "hint/V3"            (nfIO load_linear),
  bench "hint/baseline"      (nfIO hint_baseline),
  bench "hint/wumbersym"     (nfIO load_wumbersym),
  bench "hint/wumbersym_nop" (nfIO load_wumbersym_nop),
  bench "hint/wumber_all"    (nfIO load_wumber_all)
  -}
  ]
