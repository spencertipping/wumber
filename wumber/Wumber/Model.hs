{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

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


import Data.Binary   (Binary)
import Data.Foldable (toList)
import GHC.Generics  (Generic, Generic1)
import Linear.V2     (V2(..))
import Linear.V3     (V3(..))

import qualified Data.Vector.Storable as VS

import Wumber.BoundingBox
import Wumber.Constraint
import Wumber.ConstraintSolver
import Wumber.DualContour
import Wumber.Fingerprint
import Wumber.Numeric
import Wumber.Symbolic
import Wumber.SymbolicJIT
import Wumber.VectorConversion


-- | Objects whose volume can be reduced to an N-dimensional isofunction. Most
--   objects should implement this because it makes it possible for Wumber to
--   derive most other interfaces.
class BoundedObject a v => FRep a v f | a -> f, a -> v where frep :: a -> Sym f R


-- | Objects whose extents are known.
class BoundedObject a v where bounding_box :: a -> BoundingBox v


-- | Objects that undergo a compilation or solving step before they can be
--   modeled. You don't have to use this typeclass, but you should if you can
--   because Wumber will use a disk cache to store outputs that are slow to
--   compute.
class (Fingerprintable a, Binary b) => Computed a b where
  compute :: a -> b

instance (AlgConstraints f R,
          Fingerprintable (Constrained f a),
          DeterministicEval R R a b,
          Binary b) =>
         Computed (Constrained f a) b where
  compute = fst . solve 1e-6 10000

  -- FIXME
  -- Hard-coded constants


-- | The computed boundary of an 'FRep' object.
newtype ComputedBoundary v = CB { unCB :: [(v, v)] }
  deriving (Show, Eq, Generic, Binary)

-- TODO
-- This is terrible.

instance Sketch (ComputedBoundary v) v where sketch = unCB


-- | Objects that can be sketched using N-dimensional lines.
class Sketch a v where sketch :: a -> [(v, v)]

-- TODO
-- What's the canonical way to offer a foldable collection, not necessarily a
-- list? I don't want a forall; the caller doesn't get to choose.

-- TODO
-- Add level-of-detail based on view

instance (AlgConstraints f R,
          Binary (v R),
          FRep a (v R) f,
          Fingerprintable a,
          DCVector v,
          VectorConversion (v R) (VS.Vector R)) =>
         Computed a (ComputedBoundary (v R)) where

  compute o = CB $ toList $ iso_contour f bb 6 18 0.1 where f  = jit (frep o)
                                                            bb = bounding_box o

  -- FIXME
  -- Hard-coded constants above, major fail
