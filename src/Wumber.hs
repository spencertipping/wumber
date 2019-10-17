module Wumber (
  module Wumber.Cursor,
  module Wumber.Element,
  module Wumber.Sketch,
  runWumber,
  f2d, d2f,
  tau, sincos,
  mm, cm
) where

import Control.Monad.RWS.Strict
import GHC.Float

import Wumber.Cursor
import Wumber.Element
import Wumber.Sketch


f2d = float2Double
d2f = double2Float


tau      = 2 * pi
sincos θ = (sin r, cos r) where r = θ / 360 * tau

mm = (/ 25.4)
cm = (/ 2.54)


runWumber :: Cursor -> Wumber () -> [Element]
runWumber c m = snd $ execRWS m () c
