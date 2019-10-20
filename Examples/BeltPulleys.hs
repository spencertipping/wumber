{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Examples.BeltPulleys where

import Control.Monad
import Control.Monad.Identity
import Control.Monad.RWS.Strict
import Wumber
import Debug.Trace
import Graphics.Gloss
import Linear.V4


mm = (/ 25.4)


type Profile = ShapeGen ()


reducer :: Double -> Double -> Double -> Profile -> Wumber ()
reducer r1 r2 d p = do
  wheel r1 p
  jz d
  wheel r2 p


wheel :: Double -> Profile -> Wumber ()
wheel r p = spin_z 30 12 $ shape do start; lx r; p; lx (-r)


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

axle :: Double -> Double -> Wumber ()
axle od l = wheel (od/2) $ do lz l


bearing :: Double -> Double -> Double -> Wumber ()
bearing id od t = spin_z 60 6 $ shape do
  jx (id/2)
  rx 90
  start
  rect (od/2 - id/2) t

bearing_608zz    = bearing (mm 8)  (mm 22) (mm 7)
bearing_6203_2rs = bearing (mm 17) (mm 40) (mm 12)


j_teeth     = 8
belt_gap    = 0.100
wheel_depth = j_profile_depth (fromIntegral j_teeth - 1) + belt_gap * 2
wheel_gap   = 0.400
axle_od     = mm 17
axle_gap    = 0.250
axle_len    = (wheel_depth + axle_gap + mm 12) * 2 + wheel_gap

reducer_pulley :: Wumber ()
reducer_pulley = f do
  f $ reducer 3.0 1.1 (wheel_depth + wheel_gap)
                      (gapped belt_gap $ j_profile (j_teeth - 1))
  f do
    jz (-axle_gap - mm 12)
    axle axle_od axle_len
  f do
    jz (-axle_gap)
    bearing_6203_2rs
    jz (axle_len - mm 12)
    bearing_6203_2rs


stack_n       = 3
stack_spacing = 5.5


-- TODO
-- We'll want two parallel drives for the final pulley. Otherwise we'll have a
-- bunch of shear force on the frame.
pulley_stack :: Int -> Wumber ()
pulley_stack n = replicateM_ n do
  reducer_pulley
  jx stack_spacing
  rx 180
  jz (-2 * wheel_depth - wheel_gap)


frame_clearance = 0.5 + mm 40

frame_plate :: Wumber ()
frame_plate = do
  jx (-frame_clearance)
  jy (-frame_clearance)
  jz (-axle_gap)
  extrude_z 15 (-0.050) $ shape do
    start
    rect ((fromIntegral stack_n - 1) * stack_spacing + 2 * frame_clearance)
         (2 * frame_clearance)


main :: Wumber ()
main = do
  zoom 0.05
  f $ pulley_stack stack_n
  f $ frame_plate
  f do jz (axle_len + 0.750 - mm 24); frame_plate