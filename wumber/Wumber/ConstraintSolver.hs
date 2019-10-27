{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Wumber.ConstraintSolver where

import Control.Monad
import Control.Monad.ST
import Control.Monad.Reader
import Control.Monad.RWS
import Data.Array.Base (unsafeFreezeSTUArray)
import Data.Array.MArray
import Data.Array.ST
import Data.Array.Unsafe
import Data.Array.Unboxed
import Data.Bits
import qualified Data.Set as S
import Data.STRef
import GHC.Float
import Lens.Micro
import Lens.Micro.TH
import qualified Linear.Metric as LM
import Linear.V1
import Linear.V2
import Linear.V3

import Wumber.Constraint

import Debug.Trace


-- | A solved system whose values are available using 'evalM'.
type Solved a = Reader (UArray VarID N) a


-- | All independent variables used to calculate the given value.
deps :: CVal -> S.Set (VarID, N)
deps (CVar i v)            = S.singleton (i, v)
deps (CConst _)            = S.empty
deps (CLinear _ _ v)       = deps v
deps (CNonlinear xs _ _)   = S.unions (map deps xs)
deps (CNonlinearU v _ _)   = deps v
deps (CNonlinearB l r _ _) = deps l `S.union` deps r


eval :: CVal -> UArray VarID N -> N
eval (CVar i _)            xs = xs ! i
eval (CConst x)            _  = x
eval (CLinear m b v)       xs = let !x = eval v xs in m*x + b
eval (CNonlinear ops f _)  xs = f $ map (flip eval xs) ops
eval (CNonlinearU v f _)   xs = f (eval v xs)
eval (CNonlinearB l r f _) xs = eval l xs `f` eval r xs


evalM :: CVal -> Solved N
evalM v = eval v <$> ask


eval_all :: [Constraint] -> UArray VarID N -> N
eval_all cs xs = foldl (\t v -> t + eval v xs) 0 cs


constraints_from :: Constrained a -> (a, [Constraint])
constraints_from m = evalRWS m () 0


-- | Solves a system of constraints (or minimizes the error) and returns a tuple
--   of results:
--
--   > (error, remaining_iterations, solution)
--
--   We use a modified multivariate Newton's method that finds the gradient by
--   bisecting the partial derivatives for zero crossings.

solve :: Int -> N -> Constrained a -> (a, N, Int, UArray VarID N)
solve n ε m = runST do
  (v, n', xs') <- var_array >>= loop n
  return (a, v, n', xs')

  where (a, cs)   = constraints_from m
        vars      = constraint_variables cs
        ci        = indexed_by_deps cs vars
        var_array = do
          xs <- newArray (var_bounds vars) 0 :: ST s (STUArray s VarID N)
          forM_ vars \(i, v) -> writeArray xs i v
          return xs

        loop n xs = do xs' <- unsafeFreezeSTUArray xs
                       let v = eval_all cs xs'
                       if v <= ε || n <= 0
                         then return (v, n, xs')
                         else do solve_step v ci xs
                                 loop (n-1) xs


constraint_variables :: [Constraint] -> S.Set (VarID, N)
constraint_variables = S.unions . map deps

var_bounds :: S.Set (VarID, N) -> (VarID, VarID)
var_bounds = (0, ) . fst . S.findMax


-- | Constructs an index of constraints that refer to each variable. This lets
--   us minimize the amount of evaluation we need to do to calculate things like
--   partial derivatives.
indexed_by_deps :: [Constraint] -> S.Set (VarID, N) -> Array Int [Constraint]
indexed_by_deps cs vars = runSTArray do
  ix <- newArray (var_bounds vars) []
  forM_ cs \c -> do
    forM_ (deps c) \(i, _) -> do
      l <- readArray ix i
      writeArray ix i (c:l)
  return ix


-- TODO
solve_step v0 ci xs = return ()


-- | Either applies one iteration of Newton's method to this axis (if the
--   derivative sign doesn't flip), or bisects it to find a local minimum.
--   Returns the total error reduction and leaves the array in its optimized
--   state.
step_axis :: [Constraint] -> STUArray s VarID N -> VarID -> ST s N
step_axis cs xs i = do
  (!v0, !g)  <- val_partial cs i xs
  x          <- readArray xs i
  let x' = x - v0 / (if g /= 0 then g else 1)
  writeArray xs i x'
  (!v1, !g') <- val_partial cs i xs
  if signum g == signum g' && v1 < v0
    then return (v0 - v1)
    else (v0 -) <$> bisect_axis (δ x) cs xs i v0 x x' (signum g)


-- | Finds a local minimum by bisection and returns the new total error.
bisect_axis :: N -> [Constraint] -> STUArray s VarID N -> VarID
            -> N -> N -> N -> N -> ST s N
bisect_axis minδ cs xs i v0 l u lgsign
  | u - l <= minδ = return v0
  | otherwise     = do
      writeArray xs i m
      (!v, !g) <- val_partial cs i xs
      if signum g == lgsign
        then bisect_axis minδ cs xs i v  m u (signum g)
        else bisect_axis minδ cs xs i v0 l m lgsign
  where m = (l + u) / 2


-- | Produces a delta sized appropriately to the number in question: that is,
--   halfway into the mantissa. We want parameterized deltas for numerical
--   stability.
--
--   NOTE: parameterized to Double specifically, not N; we need to change the
--   constant if we change the FP precision.
δ :: Double -> Double
δ x = x * 2**(-26)


-- | Calculates the partial derivative of the given axis at the given point.
val_partial :: [Constraint] -> VarID -> STUArray s VarID N -> ST s (N, N)
val_partial cs i xs = do !v0 <- eval_all cs <$> unsafeFreezeSTUArray xs
                         x <- readArray xs i
                         let δx = δ x
                         writeArray xs i (x + δx)
                         !v <- eval_all cs <$> unsafeFreezeSTUArray xs
                         writeArray xs i x
                         return (v0, (v - v0) / δx)


{-
-- | Calculates the value and the partial derivative of the sum of specified
--   functions at the variable in question.
{-# SPECIALIZE INLINE
    partial :: [Constraint] -> (STUArray s) VarID N -> VarID -> ST s (N, N) #-}
partial :: MArray a N (ST s) => [Constraint] -> a VarID N -> VarID -> ST s (N, N)
partial cs xs i = do
  xs'  <- unsafeFreeze xs
  !v0  <- return $! foldl (\t v -> t + eval v xs') 0 cs
  !x   <- readArray xs i
  writeArray xs i (x + δ x)
  xs'' <- unsafeFreeze xs   -- Rebind to defeat subexpression caching
  !vg  <- return $! foldl (\t v -> t + eval v xs'') 0 cs
  writeArray xs i x
  return (v0, (vg - v0) / δ x)


-- | Adjusts the estimate used in Newton's method. This handles some edge cases
--   and limits the step size when the gradient is very small.
--
--   If the gradient is zero, we adjust by a small amount: √δ. This won't move
--   us closer to a solution, but it might get us past the plateau.

{-# INLINE newton_adjust #-}
newton_adjust :: N -> N -> N -> (N, N)
newton_adjust v g s
  | g == 0            = (s' * sqrt (δ v), s')
  | abs g < max_slope = (abs s' * v/max_slope * signum g, s')
  | otherwise         = (abs s' * v/g, s')

  where max_slope = sqrt (δ 1)
        s'        = if | signum g == signum s -> s * 1.1
                       | signum g == 0        -> s
                       | otherwise            -> s * (-0.5)


-- | Steps Newton's method by one iteration. Updates both partials and the
--   current position.
newton_step partial var_ids xs ps ss = do
  t <- newSTRef 0
  calculate_partials t
  update_variables
  readSTRef t

  where calculate_partials t = forM_ var_ids \i -> do
          (!v, !g) <- partial xs i
          s <- readArray ss i
          let (p', s') = newton_adjust v g s
          writeArray ps i p'
          writeArray ss i s'
          modifySTRef t (+ v)

        update_variables = forM_ var_ids \i -> do
          !x <- readArray xs i
          !d <- readArray ps i
          writeArray xs i $ x - d


-- | Solves a system using up to 'n' iterations.
newton_solve n ε partial var_ids xs ps ss = do
  v <- newton_step partial var_ids xs ps ss
  if v <= ε || n <= 0
    then return (v, n)
    else newton_solve (n - 1) ε partial var_ids xs ps ss


-- | Collects constraints, sets initial values, and solves a system using a
--   modified Newton's method. Linear constraints are trivially solvable;
--   nonlinear constraints generally work but depend on initial values.
solve n ε m = runST do
  xs <- newArray (0, vmax) 0  :: ST s (STUArray s VarID N)
  ps <- newArray (0, vmax) 0  :: ST s (STUArray s VarID N)
  ss <- newArray (0, vmax) s0 :: ST s (STUArray s VarID N)
  forM_ vars \(i, v) -> writeArray xs i v

  (r, n') <- newton_solve n ε (partial cs) var_ids xs ps ss
  xs'     <- unsafeFreezeSTUArray xs
  return (r, n', xs')

  where cs      = snd $ evalRWS m () 0
        vars    = unions (map deps cs)
        vmax    = fst $ S.findMax vars
        var_ids = map fst $ S.toList vars
        s0      = 0.5 / fromIntegral (length var_ids)
-}


-- | For testing: a system is solvable iff it converges to error below the
--   epsilon and hasn't exhausted its iteration count.
solvable :: Int -> N -> Constrained a -> Bool
solvable n ε m = v <= ε && n' > 0 where (_, v, n', _) = solve n ε m
