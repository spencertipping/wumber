{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Wumber.ConstraintSimplify where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import Lens.Micro

import Wumber.Constraint


-- | 'Simplified cs v' means "a set of constraints whose variables have
--   compact 'VarID's, and you can convert each back to the original using
--   'v ! i'". 'remap_solution' does this for you once you have a 'Vector' with
--   solution values.
data Simplified = Simplified [Constraint] (V.Vector VarID)

-- | Reduces a set of constraints to a simplified constraint bundle with
--   compactly-identified variables (whose 'VarID's correspond to 'Vector'
--   indexes used by the GSL minimizer).
simplify :: [Constraint] -> Simplified
simplify cs = Simplified cs (V.generate (maxid + 1) id)
  where (maxid, _) = foldl1 max $ map (S.findMax . constraint_deps) cs


-- | Remaps a compact solution vector into the original variable space by
--   returning the list of updates that should be applied to the final solution
--   vector. We do things in terms of vector update lists because it's common
--   for constraint systems to get partitioned into multiple subproblems and
--   recombined after the fact (which isn't an operation that vectors are
--   particularly good at).
remap_solution :: V.Vector VarID -> VS.Vector N -> [(VarID, N)]
remap_solution mi xs = V.toList mi `zip` VS.toList xs


-- | Separates independent subsystems. This is the first thing we do when
--   simplifying a set of constraints.
partition_by_vars :: [Constraint] -> [[Constraint]]
partition_by_vars cs = [cs]
  where pairs = cs & map \c -> ([c], S.map fst (constraint_deps c))


-- | The full set of initial values in a constraint.
constraint_deps :: Constraint -> S.Set (VarID, N)
constraint_deps (CEqual a b)  = deps a `S.union` deps b
constraint_deps (CMinimize v) = deps v


-- | All independent variable IDs and initial values used to calculate the given
--   constraint value. This is used both to construct initial solution vectors
--   for GSL, and to figure out which constraints are independent.
deps :: CVal -> S.Set (VarID, N)
deps (CVar i v)            = S.singleton (i, v)
deps (CConst _)            = S.empty
deps (CLinear _ _ v)       = deps v
deps (CNonlinear xs _ _)   = S.unions (map deps xs)
deps (CNonlinearU v _ _)   = deps v
deps (CNonlinearB l r _ _) = deps l `S.union` deps r
