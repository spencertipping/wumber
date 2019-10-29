{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Wumber.ConstraintSolver where

import Control.Monad
import Control.Monad.RWS
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Vector.Storable as V
import Data.Vector.Storable (Vector, (!))
import Numeric.GSL.Minimization

import Wumber.Constraint


-- | All independent variables used to calculate the given value.
deps :: CVal -> S.Set (VarID, N)
deps (CVar i v)            = S.singleton (i, v)
deps (CConst _)            = S.empty
deps (CLinear _ _ v)       = deps v
deps (CNonlinear xs _ _)   = S.unions (map deps xs)
deps (CNonlinearU v _ _)   = deps v
deps (CNonlinearB l r _ _) = deps l `S.union` deps r

eval :: Vector N -> CVal -> N
eval xs (CVar i _)            = xs ! i
eval _  (CConst x)            = x
eval xs (CLinear m b v)       = let x = eval xs v in m*x + b
eval xs (CNonlinear ops f _)  = f $ map (eval xs) ops
eval xs (CNonlinearU v f _)   = f (eval xs v)
eval xs (CNonlinearB l r f _) = eval xs l `f` eval xs r


-- | The class of objects that can have constraint variables rewritten into
--   fixed values. Not all objects will preserve form when you do this; you
--   might start with constraint-friendly data structures that render themselves
--   into more optimized final types.
class Rewritable a b | a -> b where rewrite :: (CVal -> N) -> a -> b

instance Rewritable CVal N where rewrite = id
instance (Functor f, Rewritable a b) => Rewritable (f a) (f b) where
  rewrite = fmap . rewrite


-- TODO: solve by substitution when we see usable 'CEqual' constraints

-- | Solves a constrained system that returns a rewritable value, and rewrites
--   that value with the constraint solution.
solve :: Rewritable a b => N -> Int -> Constrained a -> b
solve δ n m = b where (b, _, _) = solve_full δ n m


-- | Like 'solve', but returns the solution vector and constraints alongside the
--   result. This is useful if you want to verify tolerances.
solve_full :: Rewritable a b
           => N -> Int -> Constrained a -> (b, Vector N, [Constraint])
solve_full δ n m = (rewrite (eval solution) a, solution, cs)
  where (a, cs)  = evalRWS m () 0
        solved   = concatMap (solve' δ n) $ simplify cs
        solution = V.replicate (1 + foldl1 max (map fst solved)) 0 V.// solved


-- TODO: make sure vectors are compact even if the set of VarIDs isn't
-- TODO: figure out what 'search_size' is for

-- | Solves a constrained system and returns the solution as a list of
--   '(VarID, N)' tuples. This is a low-level function used by 'solve'.
solve' :: N -> Int -> Simplified -> [(VarID, N)]
solve' δ n (Simplified maxid cs mi) = remap_solution mi xs
  where f           = eval_constraints cs
        vars        = constraints_deps cs
        start       = V.replicate (1 + maxid) 0 V.// S.toList vars
        search_size = V.replicate (1 + maxid) 1
        (xs, _)     = minimizeV NMSimplex2 δ n search_size f start


-- | Remaps a compact solution vector into the original variable space,
--   returning the list of vector updates that should be applied. We do things
--   in terms of vector update lists because it's common for constraint systems
--   to get partitioned into multiple subproblems and recombined after the fact
--   (which isn't an operation that vectors are particularly good at).
remap_solution :: V.Vector VarID -> V.Vector N -> [(VarID, N)]
remap_solution mi xs = V.toList mi `zip` V.toList xs


-- | The full set of initial values in a list of constraints.
constraints_deps :: [Constraint] -> S.Set (VarID, N)
constraints_deps = S.unions . map \case CEqual a b -> deps a `S.union` deps b
                                        CCostFn v  -> deps v


-- | The cost function for a set of constraints at a given solution value. This
--   is called by the GSL minimizer.
eval_constraints :: [Constraint] -> Vector N -> N
eval_constraints cs xs = L.foldl' (\t v -> t + each v) 0 cs
  where each (CEqual a b) = (eval xs a - eval xs b) ** 2
        each (CCostFn v)  = max 0 $ eval xs v


-- | 'Simplified m cs v' means "a set of constraints whose variables have
--   compact 'VarID's of which the maximum is 'm', and you can convert each back
--   to the original using 'v ! i'".
data Simplified = Simplified VarID [Constraint] (V.Vector Int)


-- | Reduces a set of constraints to a set of simplified constraint bundles,
--   each with compactly-identified variables (whose 'VarID's correspond to
--   'Vector' indexes used by the GSL minimizer).
simplify :: [Constraint] -> [Simplified]
simplify cs = [Simplified maxid cs (V.generate (maxid + 1) id)]
  where (maxid, _) = S.findMax (constraints_deps cs)
