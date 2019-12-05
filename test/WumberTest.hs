module Main where

import Test.QuickCheck

import qualified WumberTest.AMD64Asm         as A
import qualified WumberTest.BitSet           as BS
import qualified WumberTest.ConstraintSolver as CS
import qualified WumberTest.JIT              as J
import qualified WumberTest.ModelAffine      as MA
import qualified WumberTest.Symbolic         as S


main = do S.runTests
          A.runTests
          BS.runTests
          CS.runTests
          J.runTests
          MA.runTests
