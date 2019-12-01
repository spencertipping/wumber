{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | The numeric end of the constraint solver. 'solve' applies algebraic
--   simplification before running numerical minimization.
module Wumber.ConstraintSolver where


import Control.Monad.RWS        (evalRWS)
import Data.Either              (lefts, rights)
import Lens.Micro               ((&))
import Numeric.GSL.Minimization (minimizeV, MinimizeMethod(..))

import qualified Data.IntMap          as IM
import qualified Data.Set             as S
import qualified Data.Vector          as V
import qualified Data.Vector.Storable as VS

import Wumber.ClosedComparable
import Wumber.Constraint
import Wumber.ConstraintSimplify
import Wumber.ConstraintSplit
import Wumber.Numeric
import Wumber.Symbolic
import Wumber.SymbolicJIT


-- | Un-monadifies a 'Constrained' monad into a series of independent
--   subsystems, simplifying each algebraically.
ccompile :: AlgConstraints f R => Constrained f a -> (a, [Subsystem f])
ccompile m = (a, subs)
  where (a, cs) = evalRWS m () (0, IM.empty)
        subs    = subsystems init (rights cs)
        init    = V.replicate (maxid + 1) 0 V.// inits
        inits   = lefts cs
        maxid   = maximum (map fst inits)


-- | Solves a constrained system that returns a rewritable value, and rewrites
--   that value with the constraint solution.
--
--   Inputs are the desired delta, the maximum number of iterations for the
--   solver to use, and the constraint system you want to solve. Internally,
--   'solve' identifies independent subsystems and applies algebraic
--   simplification to each one before handing things off to the numerical
--   minimizer. The goal is to minimize the amount of work required for complex
--   constraint sets.

solve :: (AlgConstraints f R, Eval R R a b)
      => R -> Int -> Constrained f a -> (b, V.Vector R)
solve δ n m = (eval id (solved V.!) a, solved)
  where solved    = merge_solution_vector $ map (solve_subsystem δ n) subs
        (a, subs) = ccompile m


-- | Solves a single subsystem and returns the solution in sparse '(Int, N)'
--   tuple form. This is a low-level function used by 'solve'.
solve_subsystem :: AlgConstraints f R => R -> Int -> Subsystem f -> [(Int, R)]
solve_subsystem δ n ss = remap_solution ss xs
  where (xs, _)     = minimizeV NMSimplex2 δ n search_size f (_ss_init ss)
        f           = jit (constraint_cost (_ss_compact ss))
        search_size = VS.replicate (V.length (_ss_remap ss)) 1

-- TODO(minor): bypass minimizeV and call the C function directly. This will
-- save some Haskell/C FFI overhead, although the total impact isn't high (on
-- the order of ~100ns/iteration)


-- | The total cost for a list of constraints.
constraint_cost :: FConstraints f R => [CVal f] -> CVal f
constraint_cost cs = sum $ map (** 2) cs
