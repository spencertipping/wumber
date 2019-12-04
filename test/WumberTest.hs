module Main where

import Test.QuickCheck

import qualified WumberTest.AMD64Asm         as A
import qualified WumberTest.ConstraintSolver as CS
import qualified WumberTest.JIT              as J
import qualified WumberTest.ModelAffine      as MA


main = do
  A.runTests
  CS.runTests
  J.runTests
  MA.runTests
