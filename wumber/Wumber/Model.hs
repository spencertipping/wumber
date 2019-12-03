{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | Wumber doesn't know what types of objects you want to create, nor how
--   complex their data model is. To make this work, we define typeclasses that
--   describe various dimensions of functionality your objects can provide.
--   Everything Wumber does with respect to your logical data model is mediated
--   through these interfaces.
--
--   If you're reading this because you're writing a new object type and
--   wondering what you should do, I recommend implementing 'FRep' first. Wumber
--   can derive most other typeclasses from 'FRep' and 'BoundedObject'.
--
--   This module is only the most basic layer of interfacing. As Wumber gets
--   support for other domains, we'll have things like 'Wumber.ModelStructural'
--   and 'Wumber.ModelElectrical' that define more specific interfaces.

module Wumber.Model where


import Data.Foldable (toList)
import Linear.V2     (V2(..))
import Linear.V3     (V3(..))

import qualified Data.Vector.Storable as VS

import Wumber.BoundingBox
import Wumber.DualContour
import Wumber.Numeric
import Wumber.Symbolic
import Wumber.SymbolicJIT
import Wumber.VectorConversion


-- | Objects whose volume can be reduced to an N-dimensional isofunction. Most
--   objects should implement this because it makes it possible for Wumber to
--   derive most other interfaces.
class FRep a f | a -> f where frep :: a -> Sym f R


-- | Objects whose extents are known.
class BoundedObject a v where bounding_box :: a -> BoundingBox v


-- | Objects that can be sketched using N-dimensional lines.
class Sketch a v where sketch :: a -> [(v, v)]

-- TODO
-- Add level-of-detail based on view

instance (AlgConstraints f R, FRep a f, BoundedObject a (v R),
          DCVector v, VectorConversion (v R) (VS.Vector R)) =>
         Sketch a (v R) where
  sketch o = toList $ iso_contour f bb 6 18 0.1 where f  = jit (frep o)
                                                      bb = bounding_box o

  -- FIXME
  -- Hard-coded constants above, major fail
