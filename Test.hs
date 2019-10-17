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
main = ry 90 do
  screw_z 100 1 0.01 do
    ry 90
    shape $ rect 1 1
