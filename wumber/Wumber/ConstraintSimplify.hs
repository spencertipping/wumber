{-# LANGUAGE MonoLocalBinds #-}
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
import GHC.Exts           (groupWith)
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
--
--   'csimplify' returns three values:
--
--   @
--   [CVal f]        : the new, hopefully smaller set of constraints
--   IntMap (CVal f) : the full set of variable substitutions
--   IntMap R        : any variables with algebraic solutions
--   @
--
--   'csimplify' works iteratively: it reapplies itself until 'isolate' returns
--   no further substitutions.

csimplify :: AlgConstraints f R
          => [CVal f] -> ([CVal f], IntMap (CVal f), IntMap R)

csimplify cs
  | IM.null m = (cs', m, solved)
  | otherwise = let (cs'', m', s') = csimplify cs'
                in (cs'', IM.map rewrite (IM.union m m'), IM.union solved s')

  where m       = var_substitutions cs
        rewrite = eval val (var_maybe (m !?))
        solved  = IM.filter is_val m & IM.map (\([] :+ x) -> x)
        cs'     = cs & map rewrite
                     & filter (not . S.null . vars_in)


-- | Removes cycles from a list of substitutions by applying each one
--   transitively. Because this function takes an 'IntMap', we know that each
--   variable will have at most one substitution. This means that we can apply
--   the first to the second, the first and second to the third, and so forth,
--   doing a total of /n/ 'eval' operations for /n/ substitution rules.

remove_cycles :: AlgConstraints f R => IntMap (CVal f) -> IntMap (CVal f)
remove_cycles = IM.fromList . filter neq . each IM.empty . IM.toList
  where neq (x, y)           = var x /= y
        each _ []            = []
        each m ((v, s) : vs) = (v, s') : each (IM.insert v s' m) vs
          where s' = eval val (var_maybe (m !?)) s


-- | Returns a map of algebraic substitutions that can be applied to a system of
--   constraints to reduce the number of independent variables. Substitutions
--   will have no cycles.

var_substitutions :: AlgConstraints f R => [CVal f] -> IntMap (CVal f)
var_substitutions [] = IM.empty
var_substitutions (c : cs) = isos `union` var_substitutions cs'
  where isos  = map iso vars & catMaybes & IM.fromList & remove_cycles
        iso v = (v, ) <$> isolate c 0 v
        vars  = S.toList (vars_in c)
        cs'   = map (eval val (var_maybe (isos !?))) cs
