{-# LANGUAGE BlockArguments #-}

module Test where
import Control.Monad
import Graphics.Gloss

import Cur

pic x = color (makeColor 0.8 0.8 0.9 0.8) $
        runCur do
  square 100
