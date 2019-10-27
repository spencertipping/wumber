module Main where

import Test.QuickCheck

import qualified WumberTest.ConstraintSolver as CS


main = do
  CS.runTests
