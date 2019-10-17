{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Test where

import Control.Monad
import Control.Monad.Identity
import Control.Monad.RWS.Strict
import Cur
import Graphics.Gloss
import Linear.V4


main :: Cur ()
main = f do
  ry 90
  screw_z 720 1 0.001 do
    shape $ replicateM_ 3 do lx 0.2; rz 120
