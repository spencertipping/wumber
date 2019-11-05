{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Examples.Iso where

import Control.Monad
import Control.Monad.Identity
import Control.Monad.RWS.Strict
import Debug.Trace
import Graphics.Gloss
import Linear.Metric
import Linear.Matrix
import Linear.V3
import Linear.V4
import Linear.Vector

import Wumber


for  = flip map
cfor = flip concatMap


screw :: Double -> V3 Double -> V3 Double
screw dθ v@(V3 x y z) = v *! rotate_z_m (dθ * z)


-- Isofunctions for testing
sphere :: V3 R -> IsoFn (V3 R)
sphere l v = 1 - distance v l

cube :: BB3D -> IsoFn (V3 R)
cube (BB (V3 x1 y1 z1) (V3 x2 y2 z2)) (V3 x y z) =
  foldl1 min [ x - x1, x2 - x, y - y1, y2 - y, z - z1, z2 - z ]


iunion     f g v = max (f v) (g v)
iintersect f g v = min (f v) (g v)
inegate    f v   = negate (f v)


main :: Wumber ()
main = do
  zoom 0.01
  -- i <- liftIO $ iso_element 50000 model
  -- i <- liftIO $ iso_crawler 200 0.1 model
  -- tell [i]
  -- tell [iso_scan 30 model]

  tell $ iso_contour model (BB (-2) 2) 8 18 0.1

  where model v = scs v -- + cubes v * (-0.2)

        spheres = sphere 0 `iunion` sphere 0.8
        scs     = spheres `iunion` cube (BB (-1.5) (-0.5))
                          `iunion` cube (BB (-1.2) (-0.2))
        cubes   = cube (BB (-1) 1)

        cubearray     = foldl1 iunion $ map xycube coords
        coords        = cfor [-1,-0.8..1] \x -> for [-1,-0.8..1] (x,)
        xycube (x, y) = cube (BB (V3 x y 0 ^-^ 0.05) (V3 x y 0 ^+^ 0.05))
