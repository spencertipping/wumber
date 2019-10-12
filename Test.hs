{-# LANGUAGE BlockArguments #-}

module Test where
import Control.Monad
import Graphics.Gloss

import Cur


square s = do
  replicateM 80 do
    fd 500
    rt 90.5


pic x = color (makeColor 0.8 0.8 0.9 0.8)
  $ pictures [runT (square 500), Text "10.8mm"]
