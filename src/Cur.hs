{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments, TemplateHaskell #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Cur where

import Control.Monad.Identity
import Control.Monad.RWS.Strict
import Data.Map as M
import Data.Text as T
import Debug.Trace
import GHC.Float
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.IO.Interact
import Lens.Micro
import Lens.Micro.TH
import Linear.Matrix hiding (trace)
import Linear.V3
import Linear.V4
import Linear.Vector


type Cur = RWST View [Picture] Cursor Identity

data View = V { _vm     :: !(M44 Double),
                _vbox   :: !BoundingBox,
                _vmouse :: (Modifiers, Maybe Point) }
  deriving (Show)

data Cursor = C { _cl     :: (V3 Double),
                  _cm     :: (M33 Double),
                  _ccolor :: Color,
                  _cpath  :: [Text],
                  _cbind  :: (Map Text (V3 Double)) }
  deriving (Show)

data BoundingBox = BB !(V3 Double) !(V3 Double)
  deriving (Show)

makeLenses ''View
makeLenses ''Cursor
makeLenses ''BoundingBox

runCur :: View -> Cur a -> Picture
runCur v m = pictures $ snd $ execRWS m v init_cursor


instance Bounded Double where
  minBound = -(1/0)
  maxBound = 1/0

init_cursor :: Cursor
init_cursor = C (V3 0 0 0) identity (makeColor 0.8 0.8 0.9 0.8) [] M.empty


{-# INLINE pp #-}
pp :: V3 Double -> Cur (Point, Double)
pp v = do
  vm <- asks _vm
  let V4 x y z w = vm !* point v
  return $ ((double2Float (x/z), double2Float (y/z)), z)


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
transform m = modify $ cm %~ (!*! m)

d2r θ = θ / 180 * pi
cs θ = (cos r, sin r) where r = d2r θ

rx θ = transform (V3 (V3 1 0 0) (V3 0 c (-s)) (V3 0 s c)) where (c, s) = cs θ
ry θ = transform (V3 (V3 c 0 s) (V3 0 1 0) (V3 (-s) 0 c)) where (c, s) = cs θ
rz θ = transform (V3 (V3 c (-s) 0) (V3 s c 0) (V3 0 0 1)) where (c, s) = cs θ

zoom x = transform (identity !!* x)

bind :: Text -> Cur ()
bind t = do l <- gets _cl; modify $ cbind %~ M.insert t l

fork :: Text -> Cur a -> Cur a
fork p m = do
  c0 <- get
  modify $ cpath %~ (++ T.splitOn "/" p)
  v  <- m
  b  <- gets _cbind
  put $ c0 & cbind %~ M.union b
  return v

fline :: (Cursor -> V3 Double) -> Cur ()
fline f = do
  (v1, z1) <- gets _cl >>= pp
  v2 <- f <$> get
  when (z1 > 0) do
    (v2', z2') <- pp v2
    col        <- gets _ccolor
    when (z2' > 0) $ tell [color col $ Line [v1, v2']]
  modify $ cl .~ v2

lx d = fline \(C cl m _ _ _) -> cl ^+^ m^._x ^* d
ly d = fline \(C cl m _ _ _) -> cl ^+^ m^._y ^* d
lz d = fline \(C cl m _ _ _) -> cl ^+^ m^._z ^* d

jx d = do v <- gets _cl; m <- gets _cm; modify $ cl %~ (^+^ m^._x ^* d)
jy d = do v <- gets _cl; m <- gets _cm; modify $ cl %~ (^+^ m^._y ^* d)
jz d = do v <- gets _cl; m <- gets _cm; modify $ cl %~ (^+^ m^._z ^* d)

fg :: Float -> Float -> Float -> Float -> Cur ()
fg r g b a = modify $ ccolor .~ makeColor r g b a
