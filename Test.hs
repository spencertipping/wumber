{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module Test where

import Control.Monad
import Control.Monad.Identity
import Control.Monad.RWS.Strict
import Cur
import Graphics.Gloss

square sz = replicateM_ 4 do ly sz; rz 90

beam l = box 1.5 3.5 l
wall n = replicateM_ n do; fork "" do { rx 90; beam 96 }; jx 16

profile = do
  zoom 10
  rx 90
  jx 1
  ly (-1)
  lx 1
  ly (-1)
  lx (-1)
  ly (-1)

container = screw 300 1.2 profile

main = do
  zoom 0.01
  container
  fork "" do { rx 90; jz 10; container }
  jx 32
  replicateM_ 4 do
    wall 5
    ry 90
