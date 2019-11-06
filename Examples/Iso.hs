{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Examples.Iso where

import Control.Monad
import Control.Monad.Identity
import Control.Monad.RWS.Strict
import Control.Concurrent.MVar
import Criterion
import Criterion.Main
import Criterion.Main.Options
import Data.Foldable
import Debug.Trace
import Graphics.Gloss
import Linear.Metric
import Linear.Matrix
import Linear.V3
import Linear.V4
import Linear.Vector
import System.IO (stderr)
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf

import Wumber


for  = flip map
cfor = flip concatMap


screw :: Double -> V3 Double -> V3 Double
screw dθ v@(V3 x y z) = v *! rotate_z_m (dθ * z)


-- Isofunctions for testing
sphere_calls = unsafePerformIO $ newMVar 0


count_sphere_calls = True

sphere :: V3 R -> IsoFn (V3 R)
sphere l v | count_sphere_calls = unsafePerformIO do
               modifyMVar_ sphere_calls (return . (+ 1))
               return $ 1 - distance v l
           | otherwise = 1 - distance v l

cube :: BB3D -> IsoFn (V3 R)
cube (BB (V3 x1 y1 z1) (V3 x2 y2 z2)) (V3 x y z) =
  foldl' min maxBound [ x - x1, x2 - x, y - y1, y2 - y, z - z1, z2 - z ]


iunion     f g v = max (f v) (g v)
iintersect f g v = min (f v) (g v)
inegate    f v   = negate (f v)


main :: Wumber ()
main = do
  liftIO $ modifyMVar_ sphere_calls (\_ -> return 0)
  zoom 0.01

  tell $ iso_contour model (BB (-2) 2) 8 13 0.001
  tell $ traceShow (unsafePerformIO $ readMVar sphere_calls) []

  when False $ tell $ unsafePerformIO do
    runMode (Run defaultConfig Prefix []) [
      bench "sphere"   (nf (sphere 1)      (V3 0.5 1 0.3)),
      bench "cube"     (nf (cube (BB 2 3)) (V3 0.5 1 0.3)),
      bench "distance" (nf (distance 1)    (V3 0.5 1 0.3 :: V3 Double))
      ]
    return []

  where model v = scs v -- + cubes v * (-0.2)

        spheres = sphere 0 `iunion` sphere 0.8
        scs     = spheres `iunion` cube (BB (-1.5) (-0.5))
                          `iunion` cube (BB (-1.2) (-0.2))
        cubes   = cube (BB (-1) 1)

        cubearray     = foldl1 iunion $ map xycube coords
        coords        = cfor [-1,-0.8..1] \x -> for [-1,-0.8..1] (x,)
        xycube (x, y) = cube (BB (V3 x y 0 ^-^ 0.05) (V3 x y 0 ^+^ 0.05))
