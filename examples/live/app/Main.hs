module Main where

import Linear.V3          (V3)
import System.Environment (getArgs)

import Wumber.MathFn
import Wumber.Model
import WumberShell


main :: IO ()
main = do file:_ <- getArgs
          wumber_live "src" file "model" (type_is :: FRep V3 MathFn)
