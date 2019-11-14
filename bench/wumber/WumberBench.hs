module Main where

import Criterion
import Criterion.Main

import Wumber

import qualified WumberBench.Contour      as C
import qualified WumberBench.HandcodedFns as HF
import qualified WumberBench.HintLoad     as HL


main = defaultMain bs
  where bs = concat [ HF.benchmarks,
                      HL.benchmarks,
                      C.benchmarks ]
