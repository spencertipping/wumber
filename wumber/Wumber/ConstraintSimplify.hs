{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -funbox-strict-fields -Wincomplete-patterns #-}

-- | Algebraic constraint simplification. The goal here is to reduce the number
--   of variables by substitution.
module Wumber.ConstraintSimplify where


import Data.IntMap.Strict (IntMap(..), (!?), fromList, union)
import Data.Maybe         (catMaybes, fromMaybe)
import Lens.Micro         ((&))

import qualified Data.IntMap.Strict as IM
import qualified Data.Set           as S

import Wumber.Constraint
import Wumber.Numeric
import Wumber.Symbolic
import Wumber.SymbolicAlgebra


-- | Tries to reduce the number of independent variables within a set of
--   constraints by applying algebraic substitutions to variables that can be
--   isolated.
reduce_constraints :: CConstraints f
                   => [Constraint f] -> ([Constraint f], IntMap (CVal f))
reduce_constraints cs = (cs', m)
  where m   = var_substitutions cs
        cs' = cs & map (eval val (var_maybe (m !?)))
                 & filter \case CEqual a b -> a /= b
                                _          -> True


-- | Removes cycles from a list of substitutions by applying each one
--   transitively. Because this function takes an 'IntMap', we know that each
--   variable will have at most one substitution. This means that we can apply
--   the first to the second, the first and second to the third, and so forth,
--   doing a total of /n/ 'eval' operations for /n/ substitution rules.

remove_cycles :: CConstraints f => IntMap (CVal f) -> IntMap (CVal f)
remove_cycles = IM.fromList . each IM.empty . IM.toList
  where each _ []            = []
        each m ((v, s) : vs) = (v, s') : each (IM.insert v s' m) vs
          where s' = eval val (var_maybe (m !?)) s


-- | Returns a map of algebraic substitutions that can be applied to a system of
--   constraints to reduce the number of independent variables. Substitutions
--   will have no cycles.

var_substitutions :: CConstraints f
                  => [Constraint f] -> IntMap (CVal f)
var_substitutions [] = IM.empty
var_substitutions (CMinimize _     : cs) = var_substitutions cs
var_substitutions (CInitialize _ _ : cs) = var_substitutions cs
var_substitutions (c@(CEqual a b)  : cs) = isos `union` var_substitutions cs'
  where isos  = remove_cycles $ IM.fromList $ catMaybes $ map iso vars
        iso v = (v, ) <$> isolate a b v
        vars  = S.toList (constraint_deps c)
        cs'   = map (eval val (\i -> fromMaybe (var i) (isos !? i))) cs
