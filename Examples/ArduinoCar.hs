{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Examples.ArduinoCar where

import Control.Monad
import Control.Monad.Identity
import Control.Monad.RWS.Strict
import Wumber
import Debug.Trace
import Graphics.Gloss
import Linear.V4


type Profile = ShapeGen ()


wheel :: Double -> Profile -> Wumber ()
wheel r p = spin_z 30 12 $ shape do start; lx r; p; lx (-r)

axle :: Double -> Double -> Wumber ()
axle od l = wheel (od/2) $ lz l


stepper_profile :: Double -> ShapeGen ()
stepper_profile sz = do
  jx (mm (-(sz - 8)/2))
  jy (mm (-sz/2))
  start
  replicateM_ 4 do
    lx  (mm (sz - 8))
    lxy (mm 4) (mm 4)
    rz  90


-- NEMA 17 dimensions from diagram on this page:
-- https://www.amazon.com/STEPPERONLINE-Stepper-Bipolar-Connector-compatible/dp/B00PNEQKC0?ref_=fsclp_pl_dp_1
nema_17 :: Wumber ()
nema_17 = f do body; shaft; screw_holes; faceplate; cables
  where body = f $ extrude_z 24 (mm 2) $ shape $ stepper_profile 42
        cables = f do
          jz (mm 40)
          jy (mm (-21))
          jx (mm (-2.5))
          rx 90
          extrude_z 10 (mm (-1)) $ shape do start; rect (mm 5) (mm 2)

        shaft = f do
          jz (mm (-2))
          axle (mm 5) (mm (-22))

        screw_holes = f do
          screw_hole_set
          jz (mm 43.5)
          screw_hole_set

        screw_hole_set = f $ replicateM_ 4 do
          rz 90
          f do jx (mm (-15.5)); jy (mm (-15.5)); screw_hole

        screw_hole = f $ axle (mm 3)  (mm 4.5)
        faceplate  = f $ axle (mm 22) (mm (-2.0))


-- Arduino board dimensions from here:
-- https://www.flickr.com/photos/johngineer/5484250200/sizes/o/in/photostream/
--
-- Referenced from USB-plug corner, plane along XY
arduino_uno :: Wumber ()
arduino_uno = f do
  f do jx 0.60; jy (-0.10); axle (mm 3.2) (-0.1)
  f do jx 0.55; jy (-2.00); axle (mm 3.2) (-0.1)
  f do jx 2.60; jy (-0.70); axle (mm 3.2) (-0.1)
  f do jx 2.60; jy (-1.80); axle (mm 3.2) (-0.1)

  f $ extrude_z 10 (-0.05) $ shape do
    jx (-0.250)
    jy (-0.825)
    jz (-0.100)
    start
    rect 0.625 0.500

  f $ extrude_z 10 (-0.01) $ shape do
    start
    lx (0.55 + 0.05 + 2.00 - 0.05)
    lxy 0.05 (-0.05)
    ly (-0.45)
    lxy 0.10 (-0.10)
    ly (-1.30)
    lxy (-0.10) (-0.10)
    ly (-0.10)
    lx (-2.60)
    ly 2.10


wheel_profile :: Profile
wheel_profile = do
  lxz (mm 1) (mm 1)
  lz (mm 10)
  lxz (mm (-1)) (mm 1)


wheels :: Wumber ()
wheels = f $ replicateM_ 2 do
  ry 180
  f do jz (mm 60); wheel 1.5 wheel_profile


motors :: Wumber ()
motors = f do
  jz (-2)
  rz 90
  nema_17
  ry 180
  jz (-4)
  nema_17


main :: Wumber ()
main = do
  zoom 0.1

  f do jz 2; jy (mm 21); rx 90; jx (-1.5); arduino_uno

  ry 90
  motors
  wheels
