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

module Cur.Cursor where

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

import Cur.Element


type Cur = RWST () [Element] Cursor Identity

-- TODO
-- Parameterize stuff like _ccolor; I don't think we want this module to depend
-- on Graphics.Gloss at all.
data Cursor = C { _cl     :: (V3 Double),
                  _cm     :: (M33 Double),
                  _ccolor :: Color }
  deriving (Show)

makeLenses ''Cursor


init_cursor :: Color -> Cursor
init_cursor c = C (V3 0 0 0) identity c


fork :: Cur a -> Cur a
fork m = do
  c <- get
  v <- m
  put c
  return v


-- | This is some magic that lets us infer a fork for do-blocks appended to
--   various commands. It works just like printf: we specify autofork behavior
--   for a type in terms of flattening it down to a Cur monad instance.
class AutoFork r where
  autofork :: Cur () -> r

instance (a ~ ()) => AutoFork (Cur a) where
  autofork m = m

-- NOTE: we need to indirectly match through m to make things like replicateM_
-- work. I don't know exactly why we need to do this.
instance (m ~ Cur (), AutoFork (Cur a)) => AutoFork (m -> Cur a) where
  autofork m x = autofork $ fork do m; x


-- NOTE: we need this definition to disambiguate types for autofork.
cmod :: (Cursor -> Cursor) -> Cur ()
cmod = modify

amod :: AutoFork r => (Cursor -> Cursor) -> r
amod = autofork . cmod


jump :: AutoFork r => V3 Double -> r
jump v = amod $ cl .~ v

transform :: AutoFork r => M33 Double -> r
transform m = amod $ cm %~ (m !*!)

d2r θ = θ / 180 * pi
cs θ = (cos r, sin r) where r = d2r θ

rx θ = transform (V3 (V3 1 0 0) (V3 0 c (-s)) (V3 0 s c)) where (c, s) = cs θ
ry θ = transform (V3 (V3 c 0 s) (V3 0 1 0) (V3 (-s) 0 c)) where (c, s) = cs θ
rz θ = transform (V3 (V3 c (-s) 0) (V3 s c 0) (V3 0 0 1)) where (c, s) = cs θ

zoom x = transform (identity !!* x)

fline f = autofork do
  v1 <- gets _cl
  v2 <- f <$> get
  c  <- gets _ccolor
  tell [L3D c v1 v2]
  amod $ cl .~ v2

lx d = fline \(C cl m _) -> cl ^+^ m^._x ^* d
ly d = fline \(C cl m _) -> cl ^+^ m^._y ^* d
lz d = fline \(C cl m _) -> cl ^+^ m^._z ^* d

lxy dx dy = fline \(C cl m _) -> cl ^+^ m^._x ^* dx ^+^ m^._y ^* dy
lyz dy dz = fline \(C cl m _) -> cl ^+^ m^._y ^* dy ^+^ m^._z ^* dz
lxz dx dz = fline \(C cl m _) -> cl ^+^ m^._x ^* dx ^+^ m^._z ^* dz

lxyz dx dy dz = fline \(C cl m _) ->
  cl ^+^ m^._x ^* dx ^+^ m^._y ^* dy ^+^ m^._z ^* dz

jx d = autofork do m <- gets _cm; cmod $ cl %~ (^+^ m^._x ^* d)
jy d = autofork do m <- gets _cm; cmod $ cl %~ (^+^ m^._y ^* d)
jz d = autofork do m <- gets _cm; cmod $ cl %~ (^+^ m^._z ^* d)

zx = ry 90
zy = rx 90

fg r g b a = amod $ ccolor .~ makeColor r g b a


box x y z = do
  lz z; jz (-z); lx x
  lz z; jz (-z); ly y
  lz z; jz (-z); lx (-x)
  lz z; jz (-z); ly (-y)
  jz z; lx x; ly y; lx (-x); ly (-y); jz (-z)

screw n θ dz m = fork $ replicateM_ n do fork m; rz θ; jz dz

ind :: Cur ()
ind = do fg 0 0.5 0.8 0.8 $ lx 1
         fg 0.5 0 0.8 0.8 $ ly 1
         fg 0.8 0.5 0 0.8 $ lz 1
