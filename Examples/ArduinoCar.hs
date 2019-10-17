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


reducer :: Double -> Double -> Double -> Profile -> Wumber ()
reducer r1 r2 d p = do
  wheel r1 p
  jz d
  wheel r2 p


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
nema_17 = f do body; shaft; screw_holes; faceplate
  where body = f $ extrude_z 24 (mm 2) $ shape $ stepper_profile 42

        shaft = f do
          jz (mm (-24))
          axle (mm 5) (mm (24 + 48))

        screw_holes = f do
          screw_hole_set
          jz (mm 43.5)
          screw_hole_set

        screw_hole_set = replicateM_ 4 do
          rz 90
          f do jx (mm (-15.5)); jy (mm (-15.5)); screw_hole

        screw_hole = f $ axle (mm 3) (mm 4.5)
        faceplate  = f $ axle (mm 22) (mm (-2.0))


main :: Wumber ()
main = do
  zoom 0.05
  nema_17
