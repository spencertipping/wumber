{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments, TemplateHaskell #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Cur.Element where

import Graphics.Gloss
import Lens.Micro
import Lens.Micro.TH
import Linear.Matrix hiding (trace)
import Linear.V3
import Linear.V4
import Linear.Vector


data Element = L3D !Color !(V3 Double) !(V3 Double)
  deriving (Show)


data BoundingBox = BB { _bmin :: !(V3 Double),
                        _bmax :: !(V3 Double) }
  deriving (Show)


instance Bounded Double where
  minBound = -(1/0)
  maxBound =   1/0

makeLenses ''BoundingBox


{-# INLINE bb_inside #-}
bb_inside :: BoundingBox -> V3 Double -> Bool
bb_inside (BB (V3 x1 y1 z1) (V3 x2 y2 z2)) (V3 x y z) =
  x1 <= x && x <= x2 &&
  y1 <= y && y <= y2 &&
  z1 <= z && z <= z2
