module Main where

import Test.QuickCheck

import qualified WumberTest.AMD64Asm         as A
import qualified WumberTest.ConstraintSolver as CS
import qualified WumberTest.JIT              as J


main = do
  A.runTests
  J.runTests
  CS.runTests
