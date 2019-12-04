{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | The numeric end of the constraint solver. 'solve' applies algebraic
--   simplification before running numerical minimization.
module Wumber.ConstraintSolver where


import Control.Monad.RWS        (evalRWS)
import Data.Either              (lefts, rights)
import Lens.Micro               ((&))
import Numeric.GSL.Minimization (minimizeVD, MinimizeMethodD(..))

import qualified Data.Set             as S
import qualified Data.Vector          as V
import qualified Data.Vector.Storable as VS

import Wumber.ClosedComparable
import Wumber.Constraint
import Wumber.ConstraintSimplify
import Wumber.ConstraintSplit
import Wumber.Numeric
import Wumber.Symbolic
import Wumber.SymbolicDerivative
import Wumber.SymbolicJIT
import Wumber.VectorConversion


-- | Solves a constrained system that returns a rewritable value, and rewrites
--   that value with the constraint solution.
--
--   Inputs are the desired delta, the maximum number of iterations for the
--   solver to use, and the constraint system you want to solve. Internally,
--   'solve' identifies independent subsystems and applies algebraic
--   simplification to each one before handing things off to the numerical
--   minimizer. The goal is to minimize the amount of work required for complex
--   constraint sets.

solve :: (AlgConstraints f R, DeterministicEval R R a b)
      => R -> Int -> Constrained f a -> (b, V.Vector R)
solve δ n m = (eval id (solved V.!) a, solved)
  where solved    = merge_solution_vector $ map (solve_subsystem δ n) subs
        (a, subs) = ccompile m


-- | Solves a single subsystem and returns the solution in sparse '(Int, N)'
--   tuple form. This is a low-level function used by 'solve'.
solve_subsystem :: AlgConstraints f R => R -> Int -> Subsystem f -> [(Int, R)]
solve_subsystem δ n ss = remap_solution ss xs
  where (xs, _) = minimizeVD VectorBFGS2 δ n 0.1 δ f f' (_ss_init ss)
        nvars   = VS.length (_ss_init ss)
        cost    = constraint_cost (_ss_compact ss)
        f       = jit cost
        f'      = gradient_function
                  $ zipWith derivative (repeat cost) [0 .. nvars-1]

-- TODO(minor): bypass minimizeV and call the C function directly. This will
-- save some Haskell/C FFI overhead, although the total impact isn't high (on
-- the order of ~100ns/iteration)


gradient_function :: (AlgConstraints f R, VectorConversion v (VS.Vector R))
                  => [CVal f] -> v -> VS.Vector R
gradient_function fs v = VS.fromListN (length fs) $ map (flip jit v) fs


-- | Un-monadifies a 'Constrained' monad into a series of independent
--   subsystems, simplifying each algebraically.
ccompile :: AlgConstraints f R => Constrained f a -> (a, [Subsystem f])
ccompile m = (a, subs)
  where (a, cs) = evalRWS m () 0
        subs    = subsystems init (rights cs)
        init    = V.replicate (maxid + 1) 0 V.// inits
        inits   = lefts cs
        maxid   = maximum (map fst inits)


-- | The total cost for a list of constraints.
constraint_cost :: FConstraints f R => [CVal f] -> CVal f
constraint_cost cs = sum (map (** 2) cs) / val (fi $ max 1 (length cs))
