{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Examples.Iso where

import Control.Monad
import Control.Monad.Identity
import Control.Monad.RWS.Strict
import Wumber
import Debug.Trace
import Graphics.Gloss
import Linear.V3
import Linear.V4
import Linear.Vector


for  = flip map
cfor = flip concatMap


main :: Wumber ()
main = do
  zoom 0.01
  i <- liftIO $ iso_element 100000 $ scs
  --i <- liftIO $ iso_crawler 300 0.05 $ scs
  tell [i]
  --tell [iso_scan 30 $ spheres]

  where spheres = sphere 0 `iunion` sphere 0.8
        scs     = spheres `iunion` cube (BB (-1.5) (-0.5))
                          `iunion` cube (BB (-1.2) (-0.2))
        cubes   = cube (BB (-1) 1)

        cubearray     = foldl1 iunion $ map xycube coords
        coords        = cfor [-1,-0.8..1] \x -> for [-1,-0.8..1] (x,)
        xycube (x, y) = cube (BB (V3 x y 0 ^-^ 0.05) (V3 x y 0 ^+^ 0.05))
