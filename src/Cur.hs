module Cur (
  module Cur.Cursor,
  module Cur.Element,
  module Cur.Sketch,
  runCur
) where

import Control.Monad.RWS.Strict

import Cur.Cursor
import Cur.Element
import Cur.Sketch


runCur :: Cursor -> Cur () -> [Element]
runCur c m = snd $ execRWS m () c
