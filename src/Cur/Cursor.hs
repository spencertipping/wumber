{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Cur.Cursor where

import Control.Monad.Identity
import Control.Monad.RWS.Strict
import GHC.Float
import Graphics.Gloss.Data.Color
import Lens.Micro
import Linear.Matrix hiding (trace, translation)
import Linear.Projection
import Linear.V2
import Linear.V3
import Linear.V4
import Linear.Vector

import Cur.Element


type Cur    = RWS () [Element] Cursor
type Cursor = M44 Double

init_cursor :: Cursor
init_cursor = identity


-- | This is some magic that lets us infer a fork for do-blocks appended to
--   various commands. It works just like printf: we specify autofork behavior
--   for a type in terms of flattening it down to a Cur monad instance.

class AutoFork x r where
  autofork :: x -> r

-- NOTE
-- We need n ~ m a here instead of repeating (m a) in the AutoFork instance.
-- Otherwise GHC will complain about (not-really-existent) ambiguity in
-- Cur.Sketch.
instance (n ~ m a, MonadState c m) => AutoFork (m a) n where
  autofork = id

instance (MonadState c m, AutoFork (m a) r) => AutoFork (m a) (m a -> r) where
  autofork m x = autofork $ fork do m; x


fork :: MonadState c m => m a -> m a
fork m = do
  c <- get
  v <- m
  put c
  return v

amod   = autofork . modify
amul m = amod (!*! m)


-- Translate
translation      v = identity & column _w .~ v
local_translate  v = amod $ (!*! translation (point v))
global_translate v = amod $ (translation (point v) !*!)

jx d = local_translate (V3 d 0 0)
jy d = local_translate (V3 0 d 0)
jz d = local_translate (V3 0 0 d)

jX d = global_translate (V3 d 0 0)
jY d = global_translate (V3 0 d 0)
jZ d = global_translate (V3 0 0 d)


-- Rotate/scale
transform33 m = amod $ _m33 %~ (m !*!)

zoom x = transform33 (identity !!* x)

swap_xz_m = identity & _x .~ V4 0 0 1 0 & _z .~ V4 1 0 0 0
swap_yz_m = identity & _y .~ V4 0 0 1 0 & _z .~ V4 0 1 0 0
swap_xy_m = identity & _x .~ V4 0 1 0 0 & _y .~ V4 1 0 0 0

rx θ = amul $ swap_xz_m !*! rotate_z_m θ !*! swap_xz_m
ry θ = amul $ swap_yz_m !*! rotate_z_m θ !*! swap_yz_m
rz θ = amul $               rotate_z_m θ

rotate_z_m θ = identity & _m22 .~ V2 (V2 c (-s)) (V2 s c)
  where (c, s) = (cos r, sin r)
        r      = θ / 180 * pi

zx = ry 90
zy = rx 90
