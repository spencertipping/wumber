{-# LANGUAGE BlockArguments #-}

{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | Algebraic simplification/rewriting for 'Sym' quantities.
module Wumber.SymbolicAlgebra where


import Wumber.Symbolic


-- TODO
-- 'Algebra' monad to collect/arrange rewrite rules and build optimized matching
-- (Also use variables whose scope is disjoint from normal, perhaps a new Sym
-- constructor)
