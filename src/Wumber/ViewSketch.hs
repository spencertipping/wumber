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
import Wumber.View
import Wumber.ViewColor


-- | The class of objects which can sketch themselves into a viewport with the
--   specified dimension of projection matrix.
class Sketchable a m where sketch :: ViewSettings m -> a -> Sketch (V2 R)


-- | A sketch of basic line elements. Most objects can be presented this way.
newtype Sketch v = Sketch { unSketch :: [SketchElement v] }
  deriving (Show, Read, Eq, Ord, Generic, Binary)

instance BoundingBoxC v a => BoundedObject (Sketch (v a)) (v a) where
  bounding_box (Sketch es) = unions $ map bounding_box es


-- | A single thing you might want to draw in a sketch, where @v@ is the vector
--   type of a point (e.g. @V3 R@).
data SketchElement v = SketchLine !v !v !Color
  deriving (Show, Read, Eq, Ord, Generic, Binary)

instance (Bounded v, ClosedComparable v) =>
         BoundedObject (SketchElement v) v where
  bounding_box (SketchLine a b _) = of_points [a, b]
