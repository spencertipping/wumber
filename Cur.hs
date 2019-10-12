{-# LANGUAGE BlockArguments #-}

module Cur where

import Control.Monad.Identity
import Control.Monad.RWS.Strict
import Graphics.Gloss


data TurtleState = TS !Point !Float
type CurM = RWST () Path TurtleState Identity

runT :: CurM a -> Picture
runT m = Line $ snd $ execRWS m () (TS (0, 0) 0)

fd :: Float -> CurM ()
fd d = do
  TS (x, y) t <- get
  let x' = x + d * cos t
      y' = y + d * sin t
  tell [(x, y), (x', y')]
  put $ TS (x', y') t

rt :: Float -> CurM ()
rt d = do
  TS (x, y) t <- get
  put $ TS (x, y) (t + d * pi / 180)

lt :: Float -> CurM ()
lt = rt . negate
