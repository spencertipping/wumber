{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments, TemplateHaskell #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Cur.Element where

import Control.Applicative
import Graphics.Gloss
import Lens.Micro
import Lens.Micro.TH
import Linear.Matrix
import Linear.V2
import Linear.V3
import Linear.V4


data Element = Multi   !BoundingBox [Element]
             | Plane   !BoundingBox !(M44 Double) [V2 Double]
             | Shape   !BoundingBox [V3 Double]
             | Replica !BoundingBox !Int !(M44 Double) Element
  deriving (Show)


data BoundingBox = BB { _bmin :: !(V3 Double),
                        _bmax :: !(V3 Double) }
  deriving (Show)


makeLenses ''BoundingBox


point2 :: V2 Double -> V4 Double
point2 (V2 x y) = V4 x y 0 1


-- | Returns an ordered list of vertices for the given element. This is used
--   during rendering to draw connecting lines between iterations of 'Replica's.
vertices :: Element -> [V3 Double]
vertices (Multi _ es)      = concatMap vertices es
vertices (Plane _ pc vs)   = map ((^._xyz) . (*! pc) . point2) vs
vertices (Shape _ vs)      = vs
vertices (Replica _ n t e) = concatMap each ts
  where each m = map ((^._xyz) . (*! m) . point) vs
        vs     = vertices e
        ts     = scanr (!*!) identity $ replicate n t


-- TODO
-- What kind of interfacing do we want to be able to interact with elements?
-- Should elements be able to observe things like the mouse position, or should
-- we reduce the interface to "you're focused" etc?
--
-- Let's figure out what we want to be able to do.
--
-- Ultimately, it's about a couple of things. First, we want to be able to view
-- things from different perspectives, enable measurements, that type of thing.
-- CAD-focused features for people to build stuff.
--
-- Second, and more interestingly, we want aspects of the model to interact with
-- degrees of freedom (likely via lenses). So we might have a pre-made component
-- like a hinge that provides actuation based on user interaction. Or maybe we
-- have a slide, etc. Then the user can manipulate the state of the model while
-- they're looking at it.
--
-- There are some other use cases like exploded views that are also worth
-- considering: maybe we have an "exploded axis" pseudo-component that shows the
-- travel path of each linear element.
--
-- The other big thing is that I think cursors should have a way to emit
-- view-planes that show detail for different parts. I'm not sure how this
-- should work. Are these view-planes also interactive? Do we show
-- rulers/grids/etc? Do we decompose things into subassemblies with different
-- manufacturing instructions, break stuff down into steps?


instance Bounded Double where
  minBound = -1/0
  maxBound =  1/0


{-# INLINE bb_of #-}
bb_of :: Element -> BoundingBox
bb_of (Multi bb _)       = bb
bb_of (Plane bb _ _)     = bb
bb_of (Shape bb _)       = bb
bb_of (Replica bb _ _ _) = bb


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
