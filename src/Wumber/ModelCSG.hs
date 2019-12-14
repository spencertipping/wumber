{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | Constructive solid geometry for objects in general.
module Wumber.ModelCSG where


import Data.Binary  (Binary)
import GHC.Generics (Generic, Generic1)

import Wumber.Fingerprint


-- | A CSG operation over some type of value. Typically 'a' will be an 'FRep'
--   (since that instance is builtin) but you can define CSG instances over
--   other types as well.
data CSG a = CSGJust a
           | CSGUnion     (CSG a) (CSG a)
           | CSGIntersect (CSG a) (CSG a)
           | CSGSubtract  (CSG a) (CSG a)
  deriving (Show, Eq, Generic, Generic1, Functor, Foldable, Binary)

instance Binary a => Fingerprintable (CSG a) where
  fingerprint = binary_fingerprint
