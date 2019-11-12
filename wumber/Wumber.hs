module Wumber (
  module Wumber.AMD64Asm,
  module Wumber.BoundingBox,
  module Wumber.ClosedComparable,
  module Wumber.Cursor,
  module Wumber.DualContour,
  module Wumber.Element,
  module Wumber.JIT,
  module Wumber.JITIR,
  module Wumber.Numeric,
  module Wumber.Symbolic,

  Wumber,
  runWumber,
  f2d, d2f, fi,
  tau, sincos
) where

import Control.Monad.RWS.Strict
import GHC.Float
import Linear.V3 (V3)

import Wumber.AMD64Asm
import Wumber.BoundingBox
import Wumber.ClosedComparable
import Wumber.Cursor
import Wumber.DualContour
import Wumber.Element
import Wumber.JIT
import Wumber.JITIR
import Wumber.Numeric
import Wumber.Symbolic


-- | The 'Wumber' monad, which is how you convey state to the shell and render
--   stuff.
type Wumber = RWST () [Sym Double] Cursor IO


-- Remedial Haskell functions
f2d = float2Double
d2f = double2Float
fi  = fromIntegral


tau      = 2 * pi
sincos θ = (sin r, cos r) where r = θ / 360 * tau


runWumber :: Cursor -> Wumber () -> IO [Sym Double]
runWumber c m = snd <$> execRWST m () c
