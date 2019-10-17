{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
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

class AutoFork m r | r -> m where
  autofork :: m () -> r

instance AutoFork m (m ()) where
  autofork = id

instance MonadState c m => AutoFork m (m () -> m ()) where
  autofork m x = fork do m; x


fork :: MonadState c m => m a -> m a
fork m = do
  c <- get
  v <- m
  put c
  return v

{-
amod :: (AutoFork m r, MonadState Cursor m) => (Cursor -> Cursor) -> r
amod f = autofork $ modify f

amul :: (AutoFork m r, MonadState Cursor m) => M44 Double -> r
amul m = amod (!*! m)
-}

-- Translate
translation      v = identity & column _w .~ v
local_translate  v = autofork $ modify (!*! translation (point v))
global_translate v = autofork $ modify (translation (point v) !*!)

jx d = local_translate (V3 d 0 0)
jy d = local_translate (V3 0 d 0)
jz d = local_translate (V3 0 0 d)

jX d = global_translate (V3 d 0 0)
jY d = global_translate (V3 0 d 0)
jZ d = global_translate (V3 0 0 d)


-- Rotate/scale
transform33 m = autofork $ modify $ _m33 %~ (m !*!)

zoom x = transform33 (identity !!* x)

swap_xz_m = identity & _x .~ V4 0 0 1 0 & _z .~ V4 1 0 0 0 :: M44 Double
swap_yz_m = identity & _y .~ V4 0 0 1 0 & _z .~ V4 0 1 0 0 :: M44 Double
swap_xy_m = identity & _x .~ V4 0 1 0 0 & _y .~ V4 1 0 0 0 :: M44 Double

rx :: (MonadState Cursor m, AutoFork m r) => Double -> r
ry :: (MonadState Cursor m, AutoFork m r) => Double -> r
rz :: (MonadState Cursor m, AutoFork m r) => Double -> r
rx θ = autofork $ modify (!*! rotate_x_m θ)
ry θ = autofork $ modify (!*! rotate_y_m θ)
rz θ = autofork $ modify (!*! rotate_z_m θ)

rotate_x_m θ = swap_xz_m !*! rotate_z_m θ !*! swap_xz_m
rotate_y_m θ = swap_yz_m !*! rotate_z_m θ !*! swap_yz_m
rotate_z_m θ = identity & _m22 .~ V2 (V2 c (-s)) (V2 s c)
  where (c, s) = (cos r, sin r)
        r      = θ / 180 * pi
