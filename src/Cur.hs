module Cur (
  module Cur.Cursor,
  module Cur.Element,
  module Cur.Sketch,
  runCur,
  f2d, d2f,
  tau, sincos
) where

import Control.Monad.RWS.Strict
import GHC.Float

import Cur.Cursor
import Cur.Element
import Cur.Sketch


f2d = float2Double
d2f = double2Float


tau      = 2 * pi
sincos θ = (sin r, cos r) where r = θ / 360 * tau


runCur :: Cursor -> Cur () -> [Element]
runCur c m = snd $ execRWS m () c
