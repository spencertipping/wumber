{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments, TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Cur.Sketch where

import Control.Monad.Identity
import Control.Monad.RWS.Strict
import GHC.Float
import Graphics.Gloss.Data.Color
import Lens.Micro
import Lens.Micro.TH
import Linear.Matrix hiding (trace)
import Linear.Projection
import Linear.V3
import Linear.V4
import Linear.Vector

import Cur.Cursor
import Cur.Element


fline f = autofork do
  v1 <- gets _cl
  v2 <- f <$> get
  -- tell [L3D c v1 v2]
  amod $ cl .~ v2

lx d = fline \(C cl m) -> cl ^+^ m^._x ^* d
ly d = fline \(C cl m) -> cl ^+^ m^._y ^* d
lz d = fline \(C cl m) -> cl ^+^ m^._z ^* d

lxy dx dy = fline \(C cl m) -> cl ^+^ m^._x ^* dx ^+^ m^._y ^* dy
lyz dy dz = fline \(C cl m) -> cl ^+^ m^._y ^* dy ^+^ m^._z ^* dz
lxz dx dz = fline \(C cl m) -> cl ^+^ m^._x ^* dx ^+^ m^._z ^* dz

lxyz dx dy dz = fline \(C cl m) ->
  cl ^+^ m^._x ^* dx ^+^ m^._y ^* dy ^+^ m^._z ^* dz


box x y z = do
  lz z; jz (-z); lx x
  lz z; jz (-z); ly y
  lz z; jz (-z); lx (-x)
  lz z; jz (-z); ly (-y)
  jz z; lx x; ly y; lx (-x); ly (-y); jz (-z)

screw n θ dz m = fork $ replicateM_ n do fork m; rz θ; jz dz
