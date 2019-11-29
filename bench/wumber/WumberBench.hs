module Main where

import Criterion
import Criterion.Main

import Wumber

import qualified WumberBench.BoundingBox  as BB
import qualified WumberBench.Contour      as C
import qualified WumberBench.HandcodedFns as HF
import qualified WumberBench.HintLoad     as HL


main = defaultMain bs
  where bs = concat [ C.benchmarks,
                      BB.benchmarks,
                      HF.benchmarks,
                      HL.benchmarks ]
