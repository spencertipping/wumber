module Main where

import Test.QuickCheck

import qualified WumberTest.ConstraintSolver as CS
import qualified WumberTest.JIT              as J


main = do
  J.runTests
  CS.runTests
