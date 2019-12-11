{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE StandaloneDeriving #-}
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
import Linear.V2     (V2)
import Linear.V3     (V3)
import Linear.V4     (V4)
import GHC.Generics  (Generic, Generic1)

import qualified Data.Vector.Storable as VS

import Wumber.BoundingBox
import Wumber.Constraint
import Wumber.DualContour
import Wumber.Fingerprint
import Wumber.Functionable
import Wumber.Numeric
import Wumber.SymDerivative
import Wumber.SymExpr
import Wumber.SymMath
import Wumber.SymJIT
import Wumber.VectorConversion


-- TODO
-- Use a Reader monad with configuration lenses?


-- | Objects whose volume can be reduced to an N-dimensional isofunction. Most
--   objects should implement this because it makes it possible for Wumber to
--   derive most other interfaces.
class FReppable a v f | a -> f, a -> v where frep :: a -> FRep v f

data FRep v f = FRep { _frep_fn :: SymMathV v f R,
                       _frep_bb :: BoundingBox (v R) }
  deriving (Generic)

deriving instance (FnShow f,   Show (v R)) => Show   (FRep v f)
deriving instance (Eq     f,     Eq (v R)) => Eq     (FRep v f)
deriving instance (Binary f, Binary (v R)) => Binary (FRep v f)

instance (Binary f, Binary (v R)) => Fingerprintable (FRep v f) where
  fingerprint = binary_fingerprint

makeLenses ''FRep


-- | Objects whose extents are known.
class BoundedObject a v where bounding_box :: a -> BoundingBox v

instance BoundedObject (FRep v f) (v R) where bounding_box = _frep_bb


-- | Objects that undergo a compilation or solving step before they can be
--   modeled. You don't have to use this typeclass, but you should if you can
--   because Wumber will use a disk cache to store outputs that are slow to
--   compute.
class (Fingerprintable a, Binary b) => Computed a b where compute :: a -> b

instance {-# OVERLAPPABLE #-}
         (SymMathC f R,
          Binary f,
          Binary (g a),
          SymLift R a,
          Rewritable a,
          Eq a,
          Fingerprintable (Constrained f a),
          Binary a,
          Functor g) =>
         Computed (Constrained f (g a)) (g a) where
  compute = csolve


-- | An object sketched using a given type of point. This is used for live
--   previews.
newtype Sketch v = Sketch { unSketch :: [(v, v)] }
  deriving (Show, Eq, Generic, Binary)


-- TODO
-- Add level-of-detail based on view

type ComputedContext f v = (SymMathC f R,
                            Binary (v R),
                            Binary f,
                            Fingerprintable f,
                            DCVector v,
                            SymVars v,
                            Applicative v,
                            VectorConversion (v R) (VS.Vector R))

instance {-# OVERLAPPABLE #-} ComputedContext f v =>
         Computed (FRep v f) (Sketch (v R)) where
  compute (FRep (SymMathV f) bb) =
    Sketch $ toList $ iso_contour (jit f) f' bb 6 12 0.1
    where f' v = fmap jit (vector_derivative (SymMathV f)) <*> pure v
