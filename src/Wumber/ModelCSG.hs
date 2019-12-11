{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

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
import Wumber.SymMath


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


instance (SymMathC f R, Applicative v, ClosedComparable (v R), Ord (v R)) =>
         FReppable (CSG (FRep v f)) v f where

  frep (CSGJust x) = x

  frep (CSGIntersect x y) = FRep (SymMathV $ upper xf yf) (intersect xb yb)
    where FRep (SymMathV xf) xb = frep x
          FRep (SymMathV yf) yb = frep y

  frep (CSGUnion x y) = FRep (SymMathV $ lower xf yf) (union xb yb)
    where FRep (SymMathV xf) xb = frep x
          FRep (SymMathV yf) yb = frep y

  frep (CSGSubtract x y) = FRep (SymMathV $ upper xf (negate yf)) xb
    where FRep (SymMathV xf) xb = frep x
          FRep (SymMathV yf) _  = frep y
