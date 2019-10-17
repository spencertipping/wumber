{-# LANGUAGE NamedFieldPuns, LambdaCase, BlockArguments, TemplateHaskell #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module CurShell.Event where

import GHC.Float
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact as I
import Lens.Micro
import Linear.Matrix hiding (trace)
import Linear.V2
import Linear.V3
import Linear.V4
import Linear.Vector

import Cur
import CurShell.View


update_view :: Event -> View -> View


update_view (EventKey (Char c) Down _ _) = case c of
  'r' -> const $ init_view 1080
  'b' -> vclipz .~ 0.5
  'B' -> vclipz .~ maxBound
  'x' -> (vry .~ (-90)) . (vrx .~ 0)
  'y' -> (vry .~ 0)     . (vrx .~ 90)
  'z' -> (vry .~ 0)     . (vrx .~ 0)
  'X' -> (vry .~ 90)    . (vrx .~ 0)
  'Y' -> (vry .~ 0)     . (vrx .~ (-90))
  'Z' -> (vry .~ 180)   . (vrx .~ 0)
  'p' -> vp %~ not
  _   -> id


update_view (EventKey (MouseButton b) d m@(Modifiers s c a) p)
  | elem b [LeftButton, MiddleButton, RightButton]
    = if d == Down then vmouse .~ (m, Just b, p)
                   else vmouse .~ (m, Nothing, p)

  | (c, a) == (Up, Down) =                  -- LOD settings
      case (s, b) of
        (Up, WheelUp)   -> vlod %~ (* 1.1)
        (Up, WheelDown) -> vlod %~ (/ 1.1)
        _               -> id

  | (c, a) == (Up, Up) =                    -- zoom and Z travel
      case (s, b) of
        (Up,   WheelUp)   -> vz %~ (* 1.1)
        (Up,   WheelDown) -> vz %~ (/ 1.1)
        (Down, WheelUp)   -> translate_rel $ V3 0 0   0.01
        (Down, WheelDown) -> translate_rel $ V3 0 0 (-0.01)
        _                 -> id

  | (c, a) == (Down, Up) =                  -- clip plane settings
      case (s, b) of
        (Up,   WheelUp)   -> vclipz %~ (* 1.1)
        (Up,   WheelDown) -> vclipz %~ (/ 1.1)
        (Down, WheelUp)   -> vclipa %~ (* 1.1)
        (Down, WheelDown) -> vclipa %~ (/ 1.1)
        _                 -> id

  | otherwise = id


update_view (EventMotion p@(x, y)) = \v ->
  case _vmouse v of
    (Modifiers Up Up Up, Just LeftButton, (x0, y0)) ->
      translate_rel (V3 (f2d $ x - x0) (f2d $ y - y0) 0) v
        & vmouse._3 .~ p

    (Modifiers Down Up Up, Just LeftButton, (x0, y0)) ->
      v & vry %~ (+ f2d (x - x0) * 360 / _vsz v)
        & vrx %~ (+ f2d (y0 - y) * 360 / _vsz v)
        & vmouse._3 .~ p

    _ -> v & vmouse._3 .~ p


update_view _ = id
