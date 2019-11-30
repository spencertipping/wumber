{-# LANGUAGE NoMonomorphismRestriction #-}

module Wumber (
  module Wumber.AMD64Asm,
  module Wumber.AMD64JIT,
  module Wumber.BoundingBox,
  module Wumber.ClosedComparable,
  module Wumber.Constraint,
  module Wumber.ConstraintSolver,
  module Wumber.Cursor,
  module Wumber.DualContour,
  module Wumber.Element,
  module Wumber.JIT,
  module Wumber.JITIR,
  module Wumber.Numeric,
  module Wumber.Symbolic,
  module Wumber.SymbolicJIT,

  Wumber,
  runWumber,
  sincos
) where

import Control.Monad.RWS.Strict

import Wumber.AMD64Asm
import Wumber.AMD64JIT
import Wumber.BoundingBox
import Wumber.ClosedComparable
import Wumber.Constraint
import Wumber.ConstraintSolver
import Wumber.Cursor
import Wumber.DualContour
import Wumber.Element
import Wumber.JIT
import Wumber.JITIR
import Wumber.Numeric
import Wumber.Symbolic
import Wumber.SymbolicJIT


-- | The 'Wumber' monad, which is how you convey state to the shell and render
--   stuff.
type Wumber = RWST () [Sym () Double] Cursor IO


sincos θ = (sin r, cos r) where r = θ / 360 * τ


runWumber :: Cursor -> Wumber () -> IO [Sym () Double]
runWumber c m = snd <$> execRWST m () c
