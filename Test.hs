{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module Test where

import Control.Monad
import Control.Monad.Identity
import Control.Monad.RWS.Strict
import Cur
import Graphics.Gloss

{-
beam l = box 1.5 3.5 l
wall n = replicateM_ n do zy (beam 96); jx 16

profile = zy do
  zoom 10
  jx 0.5
  lxy 1 (-1)
  replicateM_ 10 do ly (-0.1); lx (-0.1); ly (-0.1); lx 0.1
  lx (-1)
  ly (-1)

container = screw 120 6 1 profile


main = do
  zoom 0.01
  ind
  container
  zy do jz 10; container
  jx 32
  replicateM_ 4 do
    wall 5
    ry 90
-}

main = do
  replicateM_ 3000 do
    ly 1
    rz 121
    ry 1
