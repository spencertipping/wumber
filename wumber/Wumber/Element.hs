{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments, TemplateHaskell #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Wumber.Element where

import Data.Foldable (foldl')
import Graphics.Gloss
import Lens.Micro
import Lens.Micro.TH
import Linear.Matrix
import Linear.V2
import Linear.V3
import Linear.V4

import Wumber.BoundingBox


-- TODO
-- Shape represents its points in un-transformed space, so we end up
-- re-transforming them each time we want to enumerate vertices or render stuff.
-- This seems unnecessary. Is it really that worthwhile to capture "semantic"
-- information? We don't have abstract coordinates yet so at most we could put
-- them on a grid or something.

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
