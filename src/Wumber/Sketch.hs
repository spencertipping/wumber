{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Wumber.Sketch where

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

import Wumber.Cursor
import Wumber.Element


sub :: Wumber a -> Wumber (a, Cursor)
sub m = do
  r <- ask
  c <- get
  let (v, c', w) = runRWS m r c
  tell [multi_of w]
  return (v, c')


type ShapeGen = RWS (M44 Double) [V3 Double] Cursor


shape :: ShapeGen a -> Wumber a
shape m = do
  c <- get
  let (v, vs) = evalRWS m c init_cursor
  tell [shape_of c vs]
  return v

capture :: Cursor -> Wumber a -> Wumber Element
capture c m = do
  r <- ask
  let (_, [v]) = evalRWS m r c
  return v


rect :: Double -> Double -> ShapeGen ()
rect x y = do lx x; ly y; lx (-x); ly (-y)


start :: ShapeGen ()
start = l3 0 0 0

l3 :: Double -> Double -> Double -> ShapeGen ()
l3 x y z = do
  modify (!*! translation (point $ V3 x y z))
  v <- gets $ (^. column _w)
  tell [v^._xyz]

lx x = l3 x 0 0
ly y = l3 0 y 0
lz z = l3 0 0 z

lxy x y = l3 x y 0
lxz x z = l3 x 0 z
lyz y z = l3 0 y z


-- Second-order shapes
screw_z :: Int -> Double -> Double -> Wumber a -> Wumber ()
screw_z n θ d m = do
  c <- get
  e <- capture init_cursor m
  tell [replicate_of (n + 1) c (rotate_z_m θ & _z._w .~ d) e]

extrude_z n d e = screw_z n 0 d e
spin_z    n θ e = screw_z n θ 0 e
