{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}


-- NOTE
-- This code is inspired heavily by Matt Keeter's implementation at
-- https://www.mattkeeter.com/projects/contours/. I've extended it to 3D, but
-- otherwise most of the design is the same.

module Wumber.DualContour where

import Numeric.LinearAlgebra (linearSolveSVD)
import Lens.Micro
import Lens.Micro.TH
import Linear.V3




-- TODO
-- Generalize this across dimensionality so we can reuse the logic for 2D isos.
-- Haskell should give us a fairly straightforward way to do this.

data Tree a = Root { _txyz :: Tree a, _txyZ :: Tree a,
                     _txYz :: Tree a, _txYZ :: Tree a,
                     _tXyz :: Tree a, _tXyZ :: Tree a,
                     _tXYz :: Tree a, _tXYZ :: Tree a }
            | Empty
            | Full
            | Leaf a

makeLenses ''Tree


build_tree :: BoundingBox
