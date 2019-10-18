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
import Linear.V4


main :: Wumber ()
main = do
  zoom 0.01
  s <- liftIO $ iso_element 3000 $ spheres
  tell [s]

  where spheres = sphere 0 `iunion` sphere 0.5 `iunion` cube (BB (-1) 0)
        cubes   = cube (BB (-1) 1)
