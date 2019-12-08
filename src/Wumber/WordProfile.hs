{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | Profiles for 'Wumber.SymExpr' that encode down to 'Word64' values.
module Wumber.WordProfile where


import Data.Binary  (Binary)
import Data.Bits
import Data.Word    (Word64)
import GHC.Generics (Generic)

import Wumber.SymExpr


-- | A fixed-size, fixed-layout hierarchical encoding of nested functions.

-- TODO
newtype WordProfile f = WP { unWP :: Word64 }
  deriving (Eq, Ord, Show, Generic, Binary)


instance (Enum f, Bounded f) => ProfileApply (WordProfile f) f where
  prof_val = WP 0
  prof_var = WP 1
  prof_fn f ps = error "TODO"
