{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments, TemplateHaskell #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Cur where

import Control.Monad.Identity
import Control.Monad.RWS.Strict
import Data.Map as M
import Data.Text as T hiding (transpose)
import Debug.Trace
import GHC.Float
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.IO.Interact
import Lens.Micro
import Lens.Micro.TH
import Linear.Matrix hiding (trace)
import Linear.Projection
import Linear.V3
import Linear.V4
import Linear.Vector


type Cur = RWST () [Element] Cursor Identity

data Element = L3D !Color !(V3 Double) !(V3 Double)
  deriving (Show)

data Cursor = C { _cl     :: (V3 Double),
                  _cm     :: (M33 Double),
                  _ccolor :: Color,
                  _cpath  :: [Text],
                  _cbind  :: (Map Text (V3 Double)) }
  deriving (Show)

data BoundingBox = BB { _bmin :: !(V3 Double),
                        _bmax :: !(V3 Double) }
  deriving (Show)

makeLenses ''Cursor
makeLenses ''BoundingBox

runCur :: Cur a -> [Element]
runCur m = snd $ execRWS m () init_cursor

init_cursor :: Cursor
init_cursor = C (V3 0 0 0) identity (makeColor 0.8 0.8 0.9 0.8) [] M.empty


instance Bounded Double where
  minBound = -(1/0)
  maxBound =   1/0


{-# INLINE bb_intersect #-}
bb_intersect :: BoundingBox -> V3 Double -> Bool
bb_intersect (BB (V3 x1 y1 z1) (V3 x2 y2 z2)) (V3 x y z) =
  x1 <= x && x <= x2 &&
  y1 <= y && y <= y2 &&
  z1 <= z && z <= z2

{-# INLINE jump #-}
jump :: V3 Double -> Cur ()
jump v = modify $ cl .~ v

{-# INLINE transform #-}
transform :: M33 Double -> Cur ()
transform m = modify $ cm %~ (m !*!)

d2r θ = θ / 180 * pi
cs θ = (cos r, sin r) where r = d2r θ

rx θ = transform (V3 (V3 1 0 0) (V3 0 c (-s)) (V3 0 s c)) where (c, s) = cs θ
ry θ = transform (V3 (V3 c 0 s) (V3 0 1 0) (V3 (-s) 0 c)) where (c, s) = cs θ
rz θ = transform (V3 (V3 c (-s) 0) (V3 s c 0) (V3 0 0 1)) where (c, s) = cs θ

zoom x = transform (identity !!* x)

bind :: Text -> Cur ()
bind t = do l <- gets _cl; modify $ cbind %~ M.insert t l

fork :: Cur a -> Cur a
fork m = do
  c0 <- get
  v  <- m
  b  <- gets _cbind
  put $ c0 & cbind %~ M.union b
  return v

fline :: (Cursor -> V3 Double) -> Cur ()
fline f = do
  v1 <- gets _cl
  v2 <- f <$> get
  c  <- gets _ccolor
  tell [L3D c v1 v2]
  modify $ cl .~ v2

lx d = fline \(C cl m _ _ _) -> cl ^+^ m^._x ^* d
ly d = fline \(C cl m _ _ _) -> cl ^+^ m^._y ^* d
lz d = fline \(C cl m _ _ _) -> cl ^+^ m^._z ^* d

lxy dx dy = fline \(C cl m _ _ _) -> cl ^+^ m^._x ^* dx ^+^ m^._y ^* dy
lyz dy dz = fline \(C cl m _ _ _) -> cl ^+^ m^._y ^* dy ^+^ m^._z ^* dz
lxz dx dz = fline \(C cl m _ _ _) -> cl ^+^ m^._x ^* dx ^+^ m^._z ^* dz

jx d = do v <- gets _cl; m <- gets _cm; modify $ cl %~ (^+^ m^._x ^* d)
jy d = do v <- gets _cl; m <- gets _cm; modify $ cl %~ (^+^ m^._y ^* d)
jz d = do v <- gets _cl; m <- gets _cm; modify $ cl %~ (^+^ m^._z ^* d)

zx m = fork do { ry 90; m }
zy m = fork do { rx 90; m }

fg :: Float -> Float -> Float -> Float -> Cur () -> Cur ()
fg r g b a m = fork do
  modify $ ccolor .~ makeColor r g b a
  m

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
