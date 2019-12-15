{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | Kinematic graphs defined as compositions of affine transformations. The
--   general idea is that kinematic linkages relate the endpoint coordinate
--   space to the transformation parameters of the linkages, which we represent
--   using an 'EquationSystem'.
--
--   Generalized affine transformations give you more degrees of freedom than
--   mechanical systems have; for example, there aren't any real kinematic
--   linkages that produce scaling or shearing matrices. However, we don't limit
--   the set of matrices you can use to build kinematic graphs because we aren't
--   fascists. Your warranty may be void if your kinematic linkages scale stuff,
--   but we figure you have your reasons.

module Wumber.Kinematic where


import Data.Binary  (Binary)
import GHC.Generics (Generic)

import Wumber.Affine
import Wumber.EquationSystem



