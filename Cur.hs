{-# LANGUAGE BlockArguments, TemplateHaskell #-}

module Cur where

import Control.Monad.Identity
import Control.Monad.RWS.Strict
import Graphics.Gloss
import Lens.Micro
import Lens.Micro.TH


{-
Let's design the state we want.

First, this thing is for 3D CAD -- so we're going for an interactive and
detailed view into a model, and we're likely to want measurements.
-}

data TurtleState = TS { _ts_loc   :: !Point,
                        _ts_theta :: !Float }
makeLenses ''TurtleState

type CurM = RWST () Path TurtleState Identity

runT :: CurM a -> Picture
runT m = Line $ snd $ execRWS (do tell [(0, 0)]; m) () (TS (0, 0) 0)

fd :: Float -> CurM ()
fd d = do
  TS (x, y) t <- get
  let x' = x + d * cos t
      y' = y + d * sin t
  tell [(x', y')]
  modify $ ts_loc .~ (x', y')

rt :: Float -> CurM ()
rt d = modify $ ts_theta %~ (+ d * pi / 180)

lt :: Float -> CurM ()
lt = rt . negate
