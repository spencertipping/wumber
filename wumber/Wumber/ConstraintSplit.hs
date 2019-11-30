{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | Functions to split a set of constraints into independent subsystems and
--   merge those disjoint vectors back to the original variable space once each
--   subsystem has been solved.
module Wumber.ConstraintSplit (
  subsystems,
  Subsystem(..),
  remap_solution
) where


import Data.List     (partition)
import GHC.Generics  (Generic)
import Lens.Micro    ((&))
import Lens.Micro.TH (makeLenses)

import qualified Data.Set             as S
import qualified Data.Vector          as V
import qualified Data.Vector.Storable as VS

import Wumber.Constraint
import Wumber.Numeric
import Wumber.Symbolic


-- | 'Subsystem cs v inits' means "a set of constraints whose variables have
--   compact 'Int's, and you can convert each back to the original using
--   'v ! i'". 'remap_solution' does this for you once you have a 'Vector' with
--   solution values.
--
--   'Subsystem' also collects the initial value of each variable and removes
--   those elements from the 'Constraint' list.

data Subsystem f = Subsystem { _ss_constraints :: [Constraint f],
                               _ss_remap       :: V.Vector VarID,
                               _ss_init        :: VS.Vector R }
  deriving (Show, Generic)

makeLenses ''Subsystem


-- | Separates independent subsystems. This is the first thing we do when
--   simplifying a set of constraints.
--
--   TODO
--   Algebraic simplification from 'ConstraintSimplify'

subsystems :: [Constraint f] -> [Subsystem f]
subsystems cs = cs & map (\c -> ([c], constraint_deps c))
                   & group_by_overlap
                   & map fst
                   -- Simplification goes here
                   & filter (not . null)
                   & map subsystem


-- | Reduces a set of constraints to a subsystem with compactly-identified
--   variables (whose indexes correspond to 'Vector' indexes used by the GSL
--   minimizer).
--
--   TODO
--   Actually compact the variables

subsystem :: [Constraint f] -> Subsystem f
subsystem cs = Subsystem cs remap inits
  where maxid = S.findMax $ S.unions $ map constraint_deps cs
        inits = VS.generate (maxid + 1) (const 0) VS.// ivs
        ivs   = cs & concatMap \case CInitialize i v -> [(i, v)]
                                     _               -> []

        remap = V.generate (maxid + 1) id      -- FIXME


-- | Remaps a compact solution vector into the original variable space by
--   returning the list of updates that should be applied to the final solution
--   vector. We do things in terms of vector update lists because it's common
--   for constraint systems to get partitioned into multiple subproblems and
--   recombined after the fact (which isn't an operation that vectors are
--   particularly good at).

remap_solution :: V.Vector VarID -> VS.Vector R -> [(VarID, R)]
remap_solution mi xs = V.toList mi `zip` VS.toList xs


-- | Groups values by overlapping set elements, transitively. The result is a
--   list of values whose sets are fully disjoint.
group_by_overlap :: Ord b => [([a], S.Set b)] -> [([a], S.Set b)]
group_by_overlap [] = []
group_by_overlap ((l, s) : r)
  | null inside = (l, s) : group_by_overlap r
  | otherwise   = group_by_overlap ((l', s') : outside)

  where (outside, inside) = partition (S.disjoint s . snd) r
        l'                = l ++ concatMap fst inside
        s'                = S.unions (s : map snd inside)


-- | The full set of variables referred to by a constraint.
constraint_deps :: Constraint f -> S.Set VarID
constraint_deps (CEqual a b)      = vars_in a `S.union` vars_in b
constraint_deps (CMinimize v)     = vars_in v
constraint_deps (CInitialize i _) = S.singleton i
