{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

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
--
--   Presentation is managed by 'Wumber.View'.

module Wumber.ModelFRep where


import Data.Binary   (Binary)
import Data.Set      (Set)
import Lens.Micro.TH (makeLenses)
import Linear.V2     (V2)
import Linear.V3     (V3)
import GHC.Generics  (Generic, Generic1)

import qualified Data.Set as S

import Wumber.Affine
import Wumber.BoundingBox
import Wumber.ClosedComparable
import Wumber.Fingerprint
import Wumber.Functionable
import Wumber.ModelCSG
import Wumber.Numeric
import Wumber.SymExpr
import Wumber.SymMath


-- | Objects whose volume can be reduced to an N-dimensional isofunction. Most
--   objects should implement this because it makes it possible for Wumber to
--   derive most other interfaces -- e.g. you get things like trivial sketching,
--   FEA meshing, and CSG support.
class FReppable a v f | a -> f, a -> v where frep :: a -> FRep v f


-- | An object represented by an implicit function. There are three relevant
--   aspects to objects defined this way:
--
--   - '_frep_fn': the function whose isosurface defines the object boundary.
--     Positive values are inside the object, negative values outside.
--
--   - '_frep_pts': a list of points that are known to be inside the object.
--     These are used as start points when constructing meshes. You don't have
--     to provide this, but you should if (1) your function has multiple
--     disjoint volumes, or (2) your function's volume is much smaller than the
--     volume of the bounding box.
--
--   - '_frep_bb': the outer bound to consider when meshing the object. If the
--     function volume intersects the bounding box, meshing processes will clip
--     the volume on the bounds.

data FRep v f = FRep { _frep_fn  :: SymMathV v f R,
                       _frep_pts :: Set (v R),
                       _frep_bb  :: BoundingBox (v R) }
  deriving (Generic)

makeLenses ''FRep


deriving instance (FnShow f,   Show (v R)) => Show   (FRep v f)
deriving instance (Eq     f,     Eq (v R)) => Eq     (FRep v f)
deriving instance (Binary f, Binary (v R)) => Binary (FRep v f)

instance (Binary f, Binary (v R)) => Fingerprintable (FRep v f) where
  fingerprint = binary_fingerprint

instance BoundedObject (FRep v f) (v R) where bounding_box = _frep_bb

instance SymMathC f R => Affine (FRep V2 f) AffineM2 V2 R where
  transform m (FRep f p b) = FRep (transform (fmap val m) f)
                                  (S.map (transform m) p)
                                  (transform m b)

instance SymMathC f R => Affine (FRep V3 f) AffineM3 V3 R where
  transform m (FRep f p b) = FRep (transform (fmap val m) f)
                                  (S.map (transform m) p)
                                  (transform m b)

instance (SymMathC f R, BoundingBoxC v R, Ord (v R)) =>
         FReppable (CSG (FRep v f)) v f where

  frep (CSGJust x) = x

  frep (CSGUnion x y) = FRep (SymMathV $ lower xf yf) p (union xb yb)
    where FRep (SymMathV xf) xp xb = frep x
          FRep (SymMathV yf) yp yb = frep y
          p                        = S.union xp yp

  frep (CSGIntersect x y) = FRep (SymMathV $ upper xf yf) p (intersect xb yb)
    where FRep (SymMathV xf) xp xb = frep x
          FRep (SymMathV yf) yp yb = frep y
          p                        = if S.size xp < S.size yp then xp else yp

  frep (CSGSubtract x y) = FRep (SymMathV $ upper xf (negate yf)) xp xb
    where FRep (SymMathV xf) xp xb = frep x
          FRep (SymMathV yf) _  _  = frep y
