module Wumber (
  module Wumber.BoundingBox,
  module Wumber.ClosedComparable,
  module Wumber.Cursor,
  module Wumber.DualContour,
  module Wumber.Element,
  module Wumber.Numeric,
  module Wumber.Sketch,
  module Wumber.Symbolic,
  runWumber,
  f2d, d2f, fi,
  tau, sincos
) where

import Control.Monad.RWS.Strict
import GHC.Float

import Wumber.BoundingBox
import Wumber.ClosedComparable
import Wumber.Cursor
import Wumber.DualContour
import Wumber.Element
import Wumber.Numeric
import Wumber.Sketch
import Wumber.Symbolic


-- Remedial Haskell functions
f2d = float2Double
d2f = double2Float
fi  = fromIntegral


tau      = 2 * pi
sincos θ = (sin r, cos r) where r = θ / 360 * tau


runWumber :: Cursor -> Wumber () -> IO [Element]
runWumber c m = snd <$> execRWST m () c
