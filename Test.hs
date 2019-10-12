{-# LANGUAGE BlockArguments #-}

module Test where
import Control.Monad
import Graphics.Gloss

import Cur

square sz = replicateM 500 do
  ly sz
  rz 90
  rx 0.5

pic x = color (makeColor 0.8 0.8 0.9 0.8) $
        runCur do
  square 1
