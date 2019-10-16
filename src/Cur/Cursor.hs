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
import Linear.Matrix hiding (trace, translation)
import Linear.Projection
import Linear.V2
import Linear.V3
import Linear.V4
import Linear.Vector

import Cur.Element


-- TODO
-- We sometimes want to calculate the cursor-delta for a given operation. Can we
-- do this easily?

type Cur    = RWST () [Element] Cursor Identity
type Cursor = M44 Double

init_cursor = identity


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


amod :: AutoFork r => (Cursor -> Cursor) -> r
amod = autofork . modify


translation v = identity & column _w .~ point v
local_translate  c v = amod $ (!*! translation v)
global_translate c v = amod $ (translation v !*!)


transform :: AutoFork r => M33 Double -> r
transform m = amod $ _m33 %~ (m !*!)

d2r θ = θ / 180 * pi
cs θ = (cos r, sin r) where r = d2r θ

swap_xz_m = identity & _x .~ V4 0 0 1 0 & _z .~ V4 1 0 0 0
swap_yz_m = identity & _y .~ V4 0 0 1 0 & _z .~ V4 0 1 0 0
swap_xy_m = identity & _x .~ V4 0 1 0 0 & _y .~ V4 1 0 0 0

rotate_z θ = identity & _m22 .~ V2 (V2 c (-s)) (V2 s c) where (c, s) = cs θ
rotate_y θ = swap_yz_m !*! rotate_z θ !*! swap_yz_m
rotate_x θ = swap_xz_m !*! rotate_z θ !*! swap_xz_m


{-
rx θ = transform (V3 (V3 1 0 0) (V3 0 c (-s)) (V3 0 s c)) where (c, s) = cs θ
ry θ = transform (V3 (V3 c 0 s) (V3 0 1 0) (V3 (-s) 0 c)) where (c, s) = cs θ
rz θ = transform (V3 (V3 c (-s) 0) (V3 s c 0) (V3 0 0 1)) where (c, s) = cs θ

zoom x = transform (identity !!* x)

-- FIXME
jx d = autofork do m <- gets _cm; cmod $ cl %~ (^+^ m^._x ^* d)
jy d = autofork do m <- gets _cm; cmod $ cl %~ (^+^ m^._y ^* d)
jz d = autofork do m <- gets _cm; cmod $ cl %~ (^+^ m^._z ^* d)

zx = ry 90
zy = rx 90

-}
