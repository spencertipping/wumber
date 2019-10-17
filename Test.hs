{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Test where

import Control.Monad
import Control.Monad.Identity
import Control.Monad.RWS.Strict
import Cur
import Debug.Trace
import Graphics.Gloss
import Linear.V4


type Profile = ShapeGen ()


reducer :: Double -> Double -> Double -> Profile -> Cur ()
reducer r1 r2 d p = do
  wheel r1 p
  jz d
  wheel r2 p


wheel :: Double -> Profile -> Cur ()
wheel r p = spin_z 31 12 $ shape do start; lx r; p; lx (-r)


gapped :: Double -> Profile -> Profile
gapped d p = do lz d; p; lz d

j_profile_depth :: Double -> Double
j_profile_depth = ((3/32.0) *)

j_profile :: Int -> Profile
j_profile n = replicateM_ n do
  -- 3/32" ~ 0.094" between teeth
  -- 40° taper (70° from the drive axis)
  -- 0.071" rib depth
  lz  (tooth_h / 2)
  lxz 0.071 slope_h
  lxz (-0.071) slope_h
  lz  (tooth_h / 2)

  where (s, c)      = sincos 20
        slope_hypot = 0.071 / c
        slope_h     = slope_hypot * s
        tooth_h     = j_profile_depth 1 - 2 * slope_h

axle :: Double -> Double -> Cur ()
axle r l = wheel r $ do lz l


mm :: Double -> Double
mm = (/ 25.4)

-- 608ZZ: 8mm bore, 22mm OD, 7mm thickness
bearing_608zz :: Cur ()
bearing_608zz = spin_z 61 6 $ shape do
  jx (mm 8)
  rx 90     -- swap Y and Z
  start
  rect (mm 14) (mm 7)


reducer_pulley :: Cur ()
reducer_pulley = f do
  reducer 3.0 1.5 wdepth (gapped bgap $ j_profile 6)
  f do jz (-wdepth - wgap - mm 7); axle 0.375 alen
  f do jz (-wdepth - wgap); bearing_608zz; jz (alen - mm 7); bearing_608zz

  where wdepth = j_profile_depth 6 + bgap * 2
        alen   = (wdepth + wgap + mm 7) * 2
        bgap   = 0.100
        wgap   = 0.150


main :: Cur ()
main = do
  zoom 1
  reducer_pulley
  jx 6.5
  f do ry 180; jz (2 * (-j_profile_depth 6 - 0.200)); reducer_pulley

  jx 6.5
  reducer_pulley
