{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | Functions to split a set of constraints into independent subsystems and
--   merge those disjoint vectors back to the original variable space once each
--   subsystem has been solved.
module Wumber.ConstraintSplit (
  subsystems,
  Subsystem(..),
  merge_solution_vector,
  remap_solution
) where


import Data.IntMap   (IntMap)
import Data.IntSet   (IntSet)
import Data.List     (partition)
import Data.Tuple    (swap)
import GHC.Generics  (Generic)
import Lens.Micro    ((&))
import Lens.Micro.TH (makeLenses)

import qualified Data.IntMap          as IM
import qualified Data.IntSet          as IS
import qualified Data.Vector          as V
import qualified Data.Vector.Storable as VS

import Wumber.Constraint
import Wumber.ConstraintSimplify
import Wumber.Numeric
import Wumber.Symbolic


-- | A subsystem is a set of mutually dependent constraints that is ready to be
--   solved numerically. Subsystems have been simplified algebraically and any
--   solved variables are factored into the '_ss_solved' map; as a result,
--   '_ss_constraints' may be empty, in which case no numerical step is
--   required.
--
--   Because the numerical solver operates on a compactly-indexed vector, we
--   need to transform our variable indexes to remove holes. That means
--   '_ss_constraints' doesn't contain the original symbolic quantities; they're
--   transformed to use fewer variables. '_ss_remap' is a lookup table to
--   retrieve the original sparse IDs from our compact ones.
--
--   Here's which field uses which variable ID space:
--
--   @
--   _ss_constraints : original (sparse)
--   _ss_compact     : compact
--   _ss_subst       : original (sparse)
--   _ss_solved      : original (sparse)
--   _ss_remap       : compact -> original
--   _ss_init        : compact
--   @

data Subsystem f = Subsystem { _ss_constraints :: [CVal f],
                               _ss_compact     :: [CVal f],
                               _ss_subst       :: IntMap (CVal f),
                               _ss_solved      :: IntMap R,
                               _ss_remap       :: V.Vector VarID,
                               _ss_init        :: VS.Vector R }
  deriving (Show, Generic)

makeLenses ''Subsystem


-- | Takes a list of individual subsystem solutions and produces a single vector
--   in the original variable space.
merge_solution_vector :: [[(VarID, R)]] -> V.Vector R
merge_solution_vector vs = V.replicate (1 + foldl1 max (map fst c)) 0 V.// c
  where c = concat vs


-- | Separates independent subsystems. This is the first thing we do when
--   simplifying a set of constraints.
subsystems :: AlgConstraints f R => V.Vector R -> [CVal f] -> [Subsystem f]
subsystems init cs = cs & map normalize
                        & map (\c -> ([c], vars_in c))
                        & group_by_overlap
                        & map (subsystem init . fst)


-- | Reduces a set of constraints to a subsystem with compactly-identified
--   variables (whose indexes correspond to 'Vector' indexes used by the GSL
--   minimizer).
subsystem :: AlgConstraints f R => V.Vector R -> [CVal f] -> Subsystem f
subsystem init cs = Subsystem cs' compact_cs subst solved remap init'
  where (cs', subst, solved) = csimplify cs
        (m, remap)           = compact_var_mapping (IS.unions (map vars_in cs'))
        compact_cs           = map (eval val (var . (m IM.!))) cs'
        init'                = VS.generate (V.length remap)
                                           ((init V.!) . (remap V.!))


-- | Removes holes from a set of variable IDs, producing a compact set suitable
--   for numerical solving. Also returns the reverse mapping, which will become
--   '_ss_remap' in 'Subsystem'.
compact_var_mapping :: IntSet -> (IntMap VarID, V.Vector VarID)
compact_var_mapping s = (IM.fromList (map swap pairs),
                         V.replicate (length pairs) 0 V.// pairs)
  where pairs = zip [0..] (IS.toAscList s)


-- | Remaps a compact solution vector into the original variable space by
--   returning the list of updates that should be applied to the final solution
--   vector. We do things in terms of vector update lists because it's common
--   for constraint systems to get partitioned into multiple subproblems and
--   recombined after the fact (which isn't an operation that vectors are
--   particularly good at).
--
--   This function produces a list suitable for use by 'merge_solution_vector'.

remap_solution :: FConstraints f R => Subsystem f -> VS.Vector R -> [(VarID, R)]
remap_solution ss xs = algebraic ++ subst ++ numerical
  where numerical = V.toList (_ss_remap ss) `zip` VS.toList xs
        algebraic = IM.toList (_ss_solved ss)
        knowns    = IM.union (_ss_solved ss) (IM.fromList numerical)
        subst     = IM.toList $ IM.map (eval id (knowns IM.!)) (_ss_subst ss)


-- | Groups values by overlapping set elements, transitively. The result is a
--   list of values whose sets are fully disjoint.
group_by_overlap :: [([a], IntSet)] -> [([a], IntSet)]
group_by_overlap [] = []
group_by_overlap ((l, s) : r)
  | null inside = (l, s) : group_by_overlap r
  | otherwise   = group_by_overlap ((l', s') : outside)

  where (outside, inside) = partition (IS.disjoint s . snd) r
        l'                = l ++ concatMap fst inside
        s'                = IS.unions (s : map snd inside)
