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
import Wumber.Numeric
import Wumber.Symbolic


-- | 'Simplified cs v inits' means "a set of constraints whose variables have
--   compact 'Int's, and you can convert each back to the original using
--   'v ! i'". 'remap_solution' does this for you once you have a 'Vector' with
--   solution values.
--
--   'Simplified' also collects the initial value of each variable and removes
--   those elements from the 'Constraint' list.
data Simplified = Simplified [Constraint] (V.Vector Int) (VS.Vector R)


-- | Reduces a set of constraints to a simplified constraint bundle with
--   compactly-identified variables (whose 'VarID's correspond to 'Vector'
--   indexes used by the GSL minimizer).
simplify :: [Constraint] -> Simplified
simplify cs = Simplified cs (V.generate (maxid + 1) id) inits
  where maxid = foldl1 max $ map (S.findMax . constraint_deps) cs
        inits = VS.generate (maxid + 1) (const 0) VS.// ivs
        ivs   = cs & concatMap \case CInitialize i v -> [(i, v)]
                                     _               -> []


-- | Remaps a compact solution vector into the original variable space by
--   returning the list of updates that should be applied to the final solution
--   vector. We do things in terms of vector update lists because it's common
--   for constraint systems to get partitioned into multiple subproblems and
--   recombined after the fact (which isn't an operation that vectors are
--   particularly good at).
remap_solution :: V.Vector Int -> VS.Vector R -> [(Int, R)]
remap_solution mi xs = V.toList mi `zip` VS.toList xs


-- | Separates independent subsystems. This is the first thing we do when
--   simplifying a set of constraints.
partition_by_vars :: [Constraint] -> [[Constraint]]
partition_by_vars cs = [cs]
  where pairs = cs & map \c -> ([c], constraint_deps c)


-- | The full set of initial values in a constraint.
constraint_deps :: Constraint -> S.Set Int
constraint_deps (CEqual a b)      = args_in a `S.union` args_in b
constraint_deps (CMinimize v)     = args_in v
constraint_deps (CInitialize _ _) = S.empty
