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

import Wumber.AMD64Asm
import Wumber.Constraint
import Wumber.ConstraintSimplify
import Wumber.JIT
import Wumber.JITIR
import Wumber.Numeric
import Wumber.Symbolic


-- | The class of objects that can have constraint variables rewritten into
--   fixed values. Not all objects will preserve form when you do this; you
--   might start with constraint-friendly data structures that render themselves
--   into more optimized final types.
class Rewritable a b | a -> b where rewrite :: (CVal -> R) -> a -> b

instance Rewritable CVal R where rewrite = id
instance (Functor f, Rewritable a b) => Rewritable (f a) (f b) where
  rewrite = fmap . rewrite


-- | Solves a constrained system that returns a rewritable value, and rewrites
--   that value with the constraint solution.
solve :: Rewritable a b => R -> Int -> Constrained a -> b
solve δ n m = b where (b, _, _) = solve_full δ n m


-- | Like 'solve', but returns the solution vector and constraints alongside the
--   result. This is useful if you want to verify tolerances.
solve_full :: Rewritable a b
           => R -> Int -> Constrained a -> (b, Vector R, [Constraint])
solve_full δ n m = (rewrite (eval (solution !)) a, solution, cs)
  where solution = VS.replicate (1 + foldl1 max (map fst solved)) 0 VS.// solved
        (a, cs)  = evalRWS m () 0
        solved   = partition_by_vars cs & map simplify & concatMap (solve' δ n)


-- | Solves a constrained system and returns the solution as a list of
--   '(Int, N)' tuples. This is a low-level function used by 'solve'.
solve' :: R -> Int -> Simplified -> [(Int, R)]
solve' δ n (Simplified cs mi start) = remap_solution mi xs
  where (xs, _)     = minimizeV NMSimplex2 δ n search_size f start
        f           = eval_constraints cs
        search_size = VS.replicate (V.length mi) 1
        vars        = concatMap (S.toList . constraint_deps) cs


-- | The cost function for a set of constraints at a given solution value. This
--   is called by the GSL minimizer.
--
--   TODO: convert to JIT

eval_constraints :: [Constraint] -> Vector R -> R
eval_constraints cs xs = L.foldl' (\t v -> t + each v) 0 cs
  where each (CEqual a b)      = (eval (xs !) a - eval (xs !) b) ** 2
        each (CMinimize v)     = max 0 $ eval (xs !) v
        each (CInitialize _ _) = 0
