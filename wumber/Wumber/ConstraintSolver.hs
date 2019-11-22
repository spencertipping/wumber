{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | A hybrid algebraic and numerical constraint system solver.
module Wumber.ConstraintSolver (
  solve,
  solve_full,
  Rewritable(..),
  constraint_cost
) where


import Control.Monad.RWS        (evalRWS)
import Data.Vector.Storable     (Vector, (!))
import Lens.Micro               ((&))
import Numeric.GSL.Minimization (minimizeV, MinimizeMethod(..))

import qualified Data.Set             as S
import qualified Data.Vector          as V
import qualified Data.Vector.Storable as VS

import Wumber.ClosedComparable
import Wumber.Constraint
import Wumber.ConstraintSplit
import Wumber.Numeric
import Wumber.Symbolic
import Wumber.SymbolicJIT


-- | Solves a constrained system that returns a rewritable value, and rewrites
--   that value with the constraint solution.
--
--   Inputs are the desired delta, the maximum number of iterations for the
--   solver to use, and the constraint system you want to solve. Internally,
--   'solve' identifies independent subsystems and applies algebraic
--   simplification to each one before handing things off to the numerical
--   minimizer. The goal is to minimize the amount of work required for complex
--   constraint sets.
solve :: Rewritable a b => R -> Int -> Constrained a -> b
solve δ n m = b where (b, _, _) = solve_full δ n m


-- | Like 'solve', but returns the solution vector and constraints alongside the
--   result. This is useful if you want to verify tolerances.
solve_full :: Rewritable a b
           => R -> Int -> Constrained a -> (b, Vector R, [Constraint])
solve_full δ n m = (rewrite (eval (solution !)) a, solution, cs)
  where solution = VS.replicate (1 + foldl1 max (map fst solved)) 0 VS.// solved
        (a, cs)  = evalRWS m () 0
        solved   = concatMap (solve' δ n) (subsystems cs)


-- | The class of objects that can have constraint variables rewritten into
--   fixed values. Not all objects will preserve form when you do this; you
--   might start with constraint-friendly data structures that render themselves
--   into more optimized final types.
class Rewritable a b | a -> b where rewrite :: (CVal -> R) -> a -> b

instance Rewritable CVal R where rewrite = id

-- TODO
-- Fully generalize this; if I try now, I get fundep collisions because Sym is
-- itself a functor.
instance Functor f => Rewritable (f CVal) (f R) where
  rewrite = fmap . rewrite


-- | Solves a constrained system and returns the solution as a list of
--   '(Int, N)' tuples. This is a low-level function used by 'solve'.
solve' :: R -> Int -> Subsystem -> [(Int, R)]
solve' δ n (Subsystem cs mi start) = remap_solution mi xs
  where (xs, _)     = minimizeV NMSimplex2 δ n search_size f start
        f           = jit (constraint_cost cs)
        search_size = VS.replicate (V.length mi) 1

-- TODO: bypass minimizeV and call the C function directly. This will save some
-- Haskell/C FFI overhead, although the total impact isn't high.


-- | The total cost for a set of constraints. It's ok for this to be slow; the
--   goal is to get a symbolic quantity we can JIT into the cost function we
--   send to the minimizer.
constraint_cost :: [Constraint] -> Sym R
constraint_cost cs = sum $ concatMap each cs
  where each (CEqual a b)      = [(a - b) ** 2]
        each (CMinimize v)     = [upper 0 v]
        each (CInitialize _ _) = []
