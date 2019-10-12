{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module Test where
import Control.Monad
import Control.Monad.Identity
import Control.Monad.RWS.Strict
import Cur
import Graphics.Gloss

square sz = replicateM_ 4 do ly sz; rz 90

box x y z = do
  lz z; jz (-z); lx x
  lz z; jz (-z); ly y
  lz z; jz (-z); lx (-x)
  lz z; jz (-z); ly (-y)
  jz z; lx x; ly y; lx (-x); ly (-y); jz (-z)

beam l = box 1.5 3.5 l
wall n = replicateM_ n do; fork "" do { rx 90; beam 96 }; jx 16

main = do
  zoom 0.01
  replicateM_ 4 do
    wall 5
    fg 0.5 0.6 0.7 0.8
    ry 90
