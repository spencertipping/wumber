{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | Algebraic simplification/rewriting for 'Sym' quantities. Internally, we
--   reduce 'Sym's to a simple normal form and try to isolate variables using
--   rewriting rules. The goal is to handle easy cases so we can bypass
--   numerical optimization or reduce dimensionality.
--
--   Almost all of this comes down to two mechanisms:
--
--   1. Normalizing forms, e.g. condensing terms in linear subsystems
--   2. Isolating and substituting variables

module Wumber.SymbolicAlgebra where


import Data.Binary  (Binary(..))
import GHC.Generics (Generic(..))

import Wumber.Constraint
import Wumber.Symbolic
