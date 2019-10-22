{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments, TemplateHaskell #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Wumber.Element where

import Control.Applicative
import Graphics.Gloss
import Lens.Micro
import Lens.Micro.TH
import Linear.Matrix
import Linear.V2
import Linear.V3
import Linear.V4


-- TODO
-- Shape represents its points in un-transformed space, so we end up
-- re-transforming them each time we want to enumerate vertices or render stuff.
-- This seems unnecessary. Is it really that worthwhile to capture "semantic"
-- information? We don't have abstract coordinates yet so at most we could put
-- them on a grid or something.

data Element = Multi     !BoundingBox [Element]
             | Shape     !BoundingBox !(M44 Double) [V3 Double]
             | Replicate !BoundingBox !Int !(M44 Double) !(M44 Double) Element
  deriving (Show)


data BoundingBox = BB { _bmin :: !(V3 Double),
                        _bmax :: !(V3 Double) }
  deriving (Show)


makeLenses ''BoundingBox


multi_of :: [Element] -> Element
multi_of xs = Multi (bb_of_elements xs) xs

shape_of :: M44 Double -> [V3 Double] -> Element
shape_of m vs = Shape (bb_of_points $ map (inflate m) vs) m vs

replicate_of :: Int -> M44 Double -> M44 Double -> Element -> Element
replicate_of n m0 m e = Replicate (bb_of_points vs) n m0 m e
  where vs = vertices $ Replicate (BB 0 0) n m0 m e


{-# INLINE inflate#-}
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


instance Bounded Double where
  minBound = -1/0
  maxBound =  1/0


{-# INLINE bb_of #-}
bb_of :: Element -> BoundingBox
bb_of (Multi bb _)           = bb
bb_of (Shape bb _ _)         = bb
bb_of (Replicate bb _ _ _ _) = bb


bb_of_points :: [V3 Double] -> BoundingBox
bb_of_points vs = foldl1 bb_union $ map (\v -> BB v v) vs

bb_of_elements :: [Element] -> BoundingBox
bb_of_elements es = foldl1 bb_union $ map bb_of es


{-# INLINE bb_inside #-}
bb_inside :: BoundingBox -> V3 Double -> Bool
bb_inside (BB (V3 x1 y1 z1) (V3 x2 y2 z2)) (V3 x y z) =
  x1 <= x && x <= x2 &&
  y1 <= y && y <= y2 &&
  z1 <= z && z <= z2


{-# INLINE bb_union #-}
bb_union :: BoundingBox -> BoundingBox -> BoundingBox
bb_union (BB min1 max1) (BB min2 max2) =
  BB (liftA2 min min1 min2) (liftA2 max max1 max2)


{-# INLINE bb_intersect #-}
bb_intersect :: BoundingBox -> BoundingBox -> BoundingBox
bb_intersect (BB min1 max1) (BB min2 max2) =
  BB (liftA2 max min1 min2) (liftA2 min max1 max2)


{-# INLINE bb_exists #-}
bb_exists :: BoundingBox -> Bool
bb_exists b@(BB a _) = bb_inside b a


{-# INLINE bb_intersects #-}
bb_intersects :: BoundingBox -> BoundingBox -> Bool
bb_intersects a b = bb_exists $ bb_intersect a b
