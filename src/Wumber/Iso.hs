{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Wumber.Iso where

import Control.Monad.Identity
import Control.Monad.RWS.Strict
import GHC.Float
import Graphics.Gloss.Data.Color
import Lens.Micro
import Lens.Micro.TH
import Linear.Matrix
import Linear.Metric
import Linear.V2
import Linear.V3
import Linear.V4
import Linear.Vector
import System.Random

import Wumber.Cursor
import Wumber.Element


type Iso = V3 Double -> Double

sphere :: V3 Double -> Iso
sphere l v = 1 - distance v l

cube :: BoundingBox -> Iso
cube (BB (V3 x1 y1 z1) (V3 x2 y2 z2)) (V3 x y z) =
  foldl1 min [ x - x1, x2 - x, y - y1, y2 - y, z - z1, z2 - z ]


iunion     f g v = max (f v) (g v)
iintersect f g v = min (f v) (g v)
inegate    f v   = negate (f v)


ε :: Double
ε = 1e-8


gradient :: Iso -> V3 Double -> V3 Double
gradient i v@(V3 x y z) = V3 dx dy dz
  where v0 = i v
        dx = (i (V3 (x + ε) y z) - v0) / ε
        dy = (i (V3 x (y + ε) z) - v0) / ε
        dz = (i (V3 x y (z + ε)) - v0) / ε


-- Start someplace with a negative iso value and seek the boundary by following
-- the gradient.
boundary_from :: Iso -> V3 Double -> V3 Double
boundary_from i v = go i 0 v
  where go i n v
          | n > 50     = v
          | abs v0 > ε = go i (n+1) (v ^-^ v0/2 *^ gradient i v)
          | otherwise  = v
          where v0 = i v


big_random :: IO Double
big_random = randomRIO (-2, 2)

random_v :: IO (V3 Double)
random_v = do
  x <- big_random
  y <- big_random
  z <- big_random
  return $ V3 x y z


iso_element :: Int -> Iso -> IO Element
iso_element n i = do
  setStdGen $ mkStdGen 0
  vs <- filter (/= 0) <$> map (boundary_from i) <$> replicateM n random_v
  return $ Multi (bb_of_points vs) $ map each vs
  where each v = Shape (BB 0 1) identity [v, v ^-^ gradient i v ^* 0.1]
