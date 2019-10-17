module Cur (
  module Cur.Cursor,
  module Cur.Element,
  module Cur.Sketch,
  runCur,
  f2d, d2f
) where

import Control.Monad.RWS.Strict
import GHC.Float

import Cur.Cursor
import Cur.Element
import Cur.Sketch


f2d = float2Double
d2f = double2Float


runCur :: Cursor -> Cur () -> [Element]
runCur c m = snd $ execRWS m () c
