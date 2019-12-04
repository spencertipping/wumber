{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

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
import Lens.Micro.TH (makeLenses)
import GHC.Generics  (Generic, Generic1)

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
class FReppable a v f | a -> f, a -> v where frep :: a -> FRep v f

data FRep v f = FRep { _frep_fn :: Sym f R,
                       _frep_bb :: BoundingBox v }
  deriving (Show, Eq, Generic, Binary)

instance (Binary f, Binary v) => Fingerprintable (FRep v f) where
  fingerprint = binary_fingerprint

makeLenses ''FRep


-- | Objects whose extents are known.
class BoundedObject a v where bounding_box :: a -> BoundingBox v

instance BoundedObject (FRep v f) v where bounding_box = _frep_bb


-- | Objects that undergo a compilation or solving step before they can be
--   modeled. You don't have to use this typeclass, but you should if you can
--   because Wumber will use a disk cache to store outputs that are slow to
--   compute.
class (Fingerprintable a, Binary b) => Computed a b where compute :: a -> b

instance {-# OVERLAPPABLE #-}
         (AlgConstraints f R,
          Fingerprintable (Constrained f a),
          DeterministicEval R R a b,
          Binary b) =>
         Computed (Constrained f a) b where
  compute = fst . solve 1e-6 10000


-- | An object sketched using a given type of point. This is used for live
--   previews.
newtype Sketch v = Sketch { unSketch :: [(v, v)] }
  deriving (Show, Eq, Generic, Binary)


-- TODO
-- Add level-of-detail based on view

instance {-# OVERLAPPABLE #-}
         (AlgConstraints f R,
          Binary (v R),
          Binary f,
          DCVector v,
          VectorConversion (v R) (VS.Vector R)) =>
         Computed (FRep (v R) f) (Sketch (v R)) where
  compute (FRep f bb) = Sketch $ toList $ iso_contour (jit f) bb 6 18 0.1
