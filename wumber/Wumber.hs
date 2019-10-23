module Wumber (
  module Wumber.Cursor,
  module Wumber.Element,
  module Wumber.Iso,
  module Wumber.Sketch,
  runWumber,
  f2d, d2f, fi,
  tau, sincos
) where

import Control.Monad.RWS.Strict
import GHC.Float

import Wumber.Cursor
import Wumber.Element
import Wumber.Iso
import Wumber.Sketch


-- Remedial Haskell functions
f2d = float2Double
d2f = double2Float
fi  = fromIntegral


tau      = 2 * pi
sincos θ = (sin r, cos r) where r = θ / 360 * tau


runWumber :: Cursor -> Wumber () -> IO [Element]
runWumber c m = snd <$> execRWST m () c