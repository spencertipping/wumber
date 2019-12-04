{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | Models involving constructive solid geometry.
module Wumber.ModelCSG where


import Data.Binary  (Binary)
import GHC.Generics (Generic, Generic1)

import Wumber.BoundingBox
import Wumber.ClosedComparable
import Wumber.Fingerprint
import Wumber.Model
import Wumber.Numeric
import Wumber.Symbolic


-- | A CSG operation over some type of value. Typically 'a' will be an 'FRep'
--   (since that instance is builtin) but you can define CSG instances over
--   other types as well.
data CSG a = CSGJust a
           | CSGIntersect (CSG a) (CSG a)
           | CSGUnion     (CSG a) (CSG a)
           | CSGSubtract  (CSG a) (CSG a)
  deriving (Show, Eq, Generic, Generic1, Functor, Foldable, Binary)

instance Binary a => Fingerprintable (CSG a) where
  fingerprint = binary_fingerprint


-- TODO
-- Define trivial CSG for freppable objects that are computed to Sketch (or some
-- other bounded thing) and that don't interact. We don't need to re-evaluate
-- FReps for those, since the CSG can be lifted into the result domain.


instance (FConstraints f R, ClosedComparable v, Ord v) =>
         FReppable (CSG (FRep v f)) v f where
  frep (CSGJust x) = x

  frep (CSGIntersect x y) = FRep (upper xf yf) (intersect xb yb)
    where FRep xf xb = frep x
          FRep yf yb = frep y

  frep (CSGUnion x y) = FRep (lower xf yf) (union xb yb)
    where FRep xf xb = frep x
          FRep yf yb = frep y

  frep (CSGSubtract x y) = FRep (upper xf (negate yf)) xb
    where FRep xf xb = frep x
          FRep yf _  = frep y
