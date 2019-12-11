{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Wumber.Element where

import Data.Foldable (foldl')
import Lens.Micro
import Lens.Micro.TH
import Linear.Matrix
import Linear.V2
import Linear.V3
import Linear.V4

import Wumber.BoundingBox


-- TODO
-- Have 'Element' wrap dual contouring: dual contours are one variety of
-- elements, but others also exist. What kind of integration should we have with
-- symbolics and constraint variables? Do elements specify mutually constrained
-- quantities and/or lenses? Maybe it's as simple as parameterizing on a numeric
-- type that can be either 'Sym' or 'R'.


-- TODO
-- Are all elements 3D? Maybe we have different classes for different dimensions
-- of things. We can have sketch elements constrained by local 2D coordinates,
-- which can be lensed into 3D if we have a plane to frame them. Then we can
-- constrain everything in either local or global coordinates.


-- TODO
-- Facet by behaviors: 'Viewable', 'FiniteElements', etc. Then 'Element' doesn't
-- exist as a union type, but instead as a series of open-ended things that can
-- be lensed individually (and we can have element transformers).
--
-- Use 'Implicit' as the CSG backend; everything can be reduced to isos as a
-- last resort.


-- TODO
-- Use 'Element' for EDA components, which include 3D, 'EDALayout' and
-- 'EDAConductance'. I think we can use constraints to define the PCB layout,
-- perhaps with some hinting (or maybe not; it's NP-hard
-- https://en.wikipedia.org/wiki/Routing_(electronic_design_automation)).


-- This whole module is gonna be toast, just FYI


data Element = Multi     !BB3D [Element]
             | Shape     !BB3D !(M44 Double) [V3 Double]
             | Replicate !BB3D !Int !(M44 Double) !(M44 Double) Element
  deriving (Show, Ord, Eq)


multi_of :: [Element] -> Element
multi_of xs = Multi (bb_of_elements xs) xs

shape_of :: M44 Double -> [V3 Double] -> Element
shape_of m vs = Shape (of_points $ map (inflate m) vs) m vs

replicate_of :: Int -> M44 Double -> M44 Double -> Element -> Element
replicate_of n m0 m e = Replicate (of_points vs) n m0 m e
  where vs = vertices $ Replicate (BB 0 0) n m0 m e


{-# INLINE inflate #-}
inflate :: M44 Double -> V3 Double -> V3 Double
inflate m v = (m !* point v) ^._xyz


-- | Returns an ordered list of vertices for the given element. This is used
--   during rendering to draw connecting lines between iterations of
--   'Replicate's.
vertices :: Element -> [V3 Double]
vertices (Multi _ es)          = concatMap vertices es
vertices (Shape _ m vs)        = map (inflate m) vs
vertices (Replicate _ n m t e) = concatMap each ts
  where each m = map ((^._xyz) . (*! m) . point) vs
        vs     = vertices e
        ts     = scanr (!*!) m $ replicate n t


{-# INLINE bb_of #-}
bb_of :: Element -> BB3D
bb_of (Multi bb _)           = bb
bb_of (Shape bb _ _)         = bb
bb_of (Replicate bb _ _ _ _) = bb


bb_of_elements :: [Element] -> BB3D
bb_of_elements es = foldl' union empty $ map bb_of es
