{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TupleSections #-}

{-# OPTIONS_GHC -funbox-strict-fields -Wincomplete-patterns #-}

-- | Algebraic constraint simplification. The goal here is to reduce the number
--   of variables by substitution.
module Wumber.ConstraintSimplify where


import Wumber.Constraint
import Wumber.Numeric
import Wumber.SymAlgebra
import Wumber.SymExpr
import Wumber.SymMath


-- TODO
-- Incremental solver built on Monoid
