{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | Profiles for 'Wumber.SymExpr' that encode down to 'Word64' values. The
--   primary purpose of an inline profile is to reduce pointer dereferences
--   during pattern matching. If we think about the memory layout:
--
--   @
--                        zero pointers away
--                        |
--                        V
--   [Sym !f !xs ![SM _ _ p _]]
--            |
--            Cons h t --> Cons h t --> Nil
--                 |            |
--                 |            [Sym !f !xs ...]  <- three pointers away
--                 |
--                 [Sym !f !xs ..]                <- two pointers away
--   @
--
--   An ideal profile, then, describes a set of @!f@ values whose expected
--   pointer distance is uniformly distributed within that profile. This is
--   nontrivial because not all functions have the same arity. In general,
--   though, we can assume the arity distribution is log-normal or similar.
--
--   We also need to think about who's going to be using these profiles.
--   Single-layer destructuring isn't a use case because the @!f@ function field
--   will tell you everything you need to know. Profiles come in when you need
--   to query a node and something about its children; for instance, if you
--   wanted to find multiply nodes whose operands were additions without
--   visiting those operands.
--
--   There are two fairly compelling cases for profiles:
--
--   1. High-arity functions whose operands are sometimes of interest
--   2. Highly nested applications of low-arity functions
--
--   TODO

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
