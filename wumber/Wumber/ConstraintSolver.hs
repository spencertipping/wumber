{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
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
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import Data.Vector.Storable (Vector, (!))
import Lens.Micro
import Numeric.GSL.Minimization

import Wumber.Constraint
import Wumber.ConstraintSimplify


-- | The class of objects that can have constraint variables rewritten into
--   fixed values. Not all objects will preserve form when you do this; you
--   might start with constraint-friendly data structures that render themselves
--   into more optimized final types.
class Rewritable a b | a -> b where rewrite :: (CVal -> N) -> a -> b

instance Rewritable CVal N where rewrite = id
instance (Functor f, Rewritable a b) => Rewritable (f a) (f b) where
  rewrite = fmap . rewrite


-- | Solves a constrained system that returns a rewritable value, and rewrites
--   that value with the constraint solution.
solve :: Rewritable a b => N -> Int -> Constrained a -> b
solve δ n m = b where (b, _, _) = solve_full δ n m


-- | Like 'solve', but returns the solution vector and constraints alongside the
--   result. This is useful if you want to verify tolerances.
solve_full :: Rewritable a b
           => N -> Int -> Constrained a -> (b, Vector N, [Constraint])
solve_full δ n m = (rewrite (eval solution) a, solution, cs)
  where solution = VS.replicate (1 + foldl1 max (map fst solved)) 0 VS.// solved
        (a, cs)  = evalRWS m () 0
        solved   = concatMap (solve' δ n)
                   $ map simplify $ partition_by_unknowns cs


-- | Solves a constrained system and returns the solution as a list of
--   '(VarID, N)' tuples. This is a low-level function used by 'solve'.
solve' :: N -> Int -> Simplified -> [(VarID, N)]

-- TODO: figure out what 'search_size' is for
solve' δ n (Simplified cs mi) = remap_solution mi xs
  where (xs, _)     = minimizeV NMSimplex2 δ n search_size f start
        f           = eval_constraints cs
        start       = VS.replicate (V.length mi) 0 VS.// vars
        search_size = VS.replicate (V.length mi) 1
        vars        = concatMap (S.toList . constraint_deps) cs


-- | The cost function for a set of constraints at a given solution value. This
--   is called by the GSL minimizer.
eval_constraints :: [Constraint] -> Vector N -> N
eval_constraints cs xs = L.foldl' (\t v -> t + each v) 0 cs
  where each (CEqual a b)  = (eval xs a - eval xs b) ** 2
        each (CMinimize v) = max 0 $ eval xs v


-- | Evaluates a constrained value at a specific solution point.
eval :: Vector N -> CVal -> N
eval xs (CVar i _)            = xs ! i
eval _  (CConst x)            = x
eval xs (CLinear m b v)       = let !x = eval xs v in m*x + b
eval xs (CNonlinear ops f _)  = f $ map (eval xs) ops
eval xs (CNonlinearU v f _)   = f $! eval xs v
eval xs (CNonlinearB l r f _) = eval xs l `f` eval xs r
