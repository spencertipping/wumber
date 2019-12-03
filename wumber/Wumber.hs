{-# LANGUAGE NoMonomorphismRestriction #-}

module Wumber (
  module Wumber.BoundingBox,
  module Wumber.ClosedComparable,
  module Wumber.Constraint,
  module Wumber.Cursor,
  module Wumber.DualContour,
  module Wumber.Element,
  module Wumber.Numeric,
  module Wumber.Symbolic,
  module Wumber.VectorConversion,

  Wumber,
  runWumber,
  sincos
) where

import Control.Monad.RWS.Strict

import Wumber.BoundingBox
import Wumber.ClosedComparable
import Wumber.Constraint
import Wumber.Cursor
import Wumber.DualContour
import Wumber.Element
import Wumber.Numeric
import Wumber.Symbolic
import Wumber.VectorConversion


-- | The 'Wumber' monad, which is how you convey state to the shell and render
--   stuff.
type Wumber = RWST () [Sym () Double] Cursor IO


-- TODO
-- WTF is this doing here

sincos θ = (sin r, cos r) where r = θ / 360 * τ


runWumber :: Cursor -> Wumber () -> IO [Sym () Double]
runWumber c m = snd <$> execRWST m () c
