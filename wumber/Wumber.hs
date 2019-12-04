{-# LANGUAGE NoMonomorphismRestriction #-}

module Wumber (
  module Wumber.BoundingBox,
  module Wumber.ClosedComparable,
  module Wumber.Constraint,
  module Wumber.Cursor,
  module Wumber.Element,
  module Wumber.Fingerprint,
  module Wumber.Model,
  module Wumber.Numeric,
  module Wumber.Symbolic,

  Fingerprint,
  sincos
) where

import GHC.Fingerprint (Fingerprint)

import Wumber.BoundingBox
import Wumber.ClosedComparable
import Wumber.Constraint
import Wumber.Cursor
import Wumber.Element
import Wumber.Fingerprint
import Wumber.Model
import Wumber.Numeric
import Wumber.Symbolic


-- TODO
-- WTF is this doing here

sincos θ = (sin r, cos r) where r = θ / 360 * τ
