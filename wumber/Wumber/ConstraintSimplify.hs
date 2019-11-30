{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -funbox-strict-fields -Wincomplete-patterns #-}

-- | Algebraic constraint simplification. The goal here is to reduce the number
--   of variables by substitution.
module Wumber.ConstraintSimplify where


import Data.IntMap.Strict (IntMap(..), fromList, union)
import Data.Maybe         (catMaybes)

import qualified Data.IntMap.Strict as IM
import qualified Data.Set           as S

import Wumber.Constraint
import Wumber.Numeric
import Wumber.Symbolic
import Wumber.SymbolicAlgebra


-- | Tries to reduce the number of independent variables within a set of
--   constraints by applying algebraic substitutions to variables that can be
--   isolated.

reduce_constraints :: FConstraints f R
                   => [Constraint f] -> ([Constraint f], IntMap (Sym f R))

reduce_constraints cs = ([], IM.empty)
  where vs = var_substitutions cs


-- | Returns a map of algebraic substitutions that can be applied to a system of
--   constraints to reduce the number of independent variables. Substitutions
--   may overlap and/or create circular rewrites.

var_substitutions :: FConstraints f R => [Constraint f] -> IntMap (Sym f R)
var_substitutions [] = IM.empty
var_substitutions (CMinimize _     : cs) = var_substitutions cs
var_substitutions (CInitialize _ _ : cs) = var_substitutions cs

var_substitutions (c@(CEqual a b) : cs) = isos `union` var_substitutions cs
  where isos  = IM.fromList $ catMaybes (map iso vars)
        iso v = (v, ) <$> isolate a b v
        vars  = S.toList (constraint_deps c)
