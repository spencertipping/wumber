{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | Human-visible representations of things in 'Wumber.Model'. And by that I
--   mean, /descriptions/ of human-visible representations -- since Wumber
--   doesn't directly manage rendering.
module Wumber.ViewSketch where


import Data.Binary  (Binary)
import GHC.Generics (Generic)
import Linear.V2    (V2)
import Linear.V3    (V3)
import Linear.V4    (V4)

import Wumber.BoundingBox
import Wumber.ClosedComparable
import Wumber.Numeric


-- | The class of objects that can be presented with a sketch.
class Sketchable a v | a -> v where sketch :: a -> Sketch v


-- | A sketch of basic line elements. Most objects can be presented this way.
newtype Sketch v = Sketch { unSketch :: [SketchElement v] }
  deriving (Show, Eq, Ord, Generic, Binary)

instance BoundingBoxC v a => BoundedObject (Sketch (v a)) (v a) where
  bounding_box (Sketch es) = unions $ map bounding_box es


-- | A single thing you might want to draw in a sketch, where @v@ is the vector
--   type of a point (e.g. @V3 R@).
data SketchElement v = SketchLine !v !v !Color
  deriving (Show, Eq, Ord, Generic, Binary)

instance (Bounded v, ClosedComparable v) =>
         BoundedObject (SketchElement v) v where
  bounding_box (SketchLine a b _) = of_points [a, b]


-- | A color, specified one of several ways. In general you should use the
--   least-specific description that works for your use case because Wumber can
--   be configured to modify color spectra to accommodate colorblindness or
--   display deficiency (or accommodate reduced color spaces, e.g. from anaglyph
--   3D).
--
--   Color rendering modes are:
--
--   - 'ColorRGBADontUse': please file a github issue if you find yourself using
--     this, as it gives Wumber no way to adjust color scales for people who
--     prefer reduced colorspaces or alternative color schemes. In general,
--     Wumber colors are more about semantics than display, and things like
--     'ColorScale1' are usually easier to work with.
--
--   - 'ColorScale0' @i@: a monochromatic presentation where the point's
--     importance is @i@. Wumber will render @i@ as brightness, line thickness,
--     opacity, or some mixture of those. If you just want everything in the
--     same color, set @i@ to 1.
--
--   - 'ColorScale1' @c@ @i@: a one-dimensional spectrum along the unit interval
--     @c@ with importance @i@. @c@ will be rendered in such a way that all
--     values have approximately equal visual prominence and are maximally
--     distinct for the user.
--
--   - 'ColorScale2' @c@ @d@ @i@: a two-dimensional color spectrum. Wumber will
--     choose orthogonal basis vectors within the user's preferred colorspace
--     and project @c@ and @d@ in such a way that they are jointly distinct if
--     possible. Failing that, Wumber will present one at a time or split the
--     view and present the two dimensions next to each other.
--
--   - 'ColorScale3': kinda what you think it would be.

data Color = ColorRGBADontUse !R !R !R !R
           | ColorScale0 !R
           | ColorScale1 !R !R
           | ColorScale2 !R !R !R
           | ColorScale3 !R !R !R !R
  deriving (Show, Eq, Ord, Generic, Binary)
