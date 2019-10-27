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
import Data.Array.Base (freezeSTUArray, unsafeFreezeSTUArray)
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
                         else do solve_step v cs ci xs
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
    forM_ (deps c) \(i, _) -> do l <- readArray ix i
                                 writeArray ix i (c:l)
  return ix


-- | Locally minimizes each axis, one at a time, to find gradients. Then we
--   locally minimize the full constraint set along the gradient line.
solve_step :: N -> [Constraint] -> Array VarID [Constraint]
           -> STUArray s VarID N -> ST s ()
solve_step v0 cs ci xs = do
  b  <- getBounds xs
  gs <- newArray b 0 :: ST s (STUArray s VarID N)
  ys <- newArray b 0 :: ST s (STUArray s VarID N)

  is <- indices <$> unsafeFreezeSTUArray xs
  forM_ is \i -> do x <- readArray xs i
                    (_, g) <- val_partial cs i xs x
                    writeArray gs i (-g)

  xs' <- unsafeFreezeSTUArray xs
  gs' <- unsafeFreezeSTUArray gs
  gm  <- optimize_vector cs is xs' gs'
  forM_ is \i -> writeArray xs i (xs'!i + gm * gs'!i)


optimize_vector :: [Constraint] -> [VarID]
                -> UArray VarID N -> UArray VarID N -> ST s N
optimize_vector cs is xs gs = do
  ys         <- newArray (bounds xs) 0 :: ST s (STUArray s VarID N)
  (!lv, !lg) <- vector_gradient cs is xs gs ys 0
  (!uv, !ug) <- vector_gradient cs is xs gs ys 1
  if signum lg == signum ug
    then return 0.5
    else bisect ys 0 1 lv uv (signum lg)

  where bisect ys l u lv uv lgs
          | u - l < δ 1 = return m
          | otherwise   = do
              (mv, mg) <- vector_gradient cs is xs gs ys m
              if signum mg == lgs
                then bisect ys m u mv uv (signum mg)
                else bisect ys l m lv mv lgs
          where m = (l + u) / 2


vector_gradient :: [Constraint] -> [VarID]
                -> UArray VarID N -> UArray VarID N -> STUArray s VarID N
                -> N -> ST s (N, N)
vector_gradient cs is xs gs ys g = do
  let δg = δ g
      g' = g + δg
  forM_ is \i -> do writeArray ys i (xs!i + g * gs!i)
  !v0 <- eval_all cs <$> unsafeFreezeSTUArray ys
  forM_ is \i -> do writeArray ys i (xs!i + g' * gs!i)
  !v1 <- eval_all cs <$> unsafeFreezeSTUArray ys
  return (v0, (v1 - v0) / δg)


-- | Either applies one iteration of Newton's method to this axis (if the
--   derivative sign doesn't flip), or bisects it to find a local minimum.
--   Returns the total error reduction and leaves the array in its optimized
--   state.
optimize_axis :: [Constraint] -> STUArray s VarID N -> VarID -> ST s N
optimize_axis cs xs i = do
  x          <- readArray xs i
  (!v0, !g)  <- val_partial cs i xs x
  let x' = x - v0 / (if g /= 0 then g else 1)
  (!v1, !g') <- val_partial cs i xs x'
  if signum g == signum g'
    then return (v0 - v1)
    else (v0 -) <$> bisect_axis (δ x) cs xs i v0 x x' (signum g)


-- | Finds a local minimum by bisection and returns the new total error.
bisect_axis :: N -> [Constraint] -> STUArray s VarID N -> VarID
            -> N -> N -> N -> N -> ST s N
bisect_axis minδ cs xs i v0 l u lgsign
  | u - l <= minδ = return v0
  | otherwise     = do
      (!v, !g) <- val_partial cs i xs m
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
δ x = max ε $ abs x * ε where ε = 2**(-26)


-- | Calculates the partial derivative of the given axis at the given point.
val_partial :: [Constraint] -> VarID -> STUArray s VarID N -> N -> ST s (N, N)
val_partial cs i xs x = do writeArray xs i x
                           !v0 <- eval_all cs <$> unsafeFreezeSTUArray xs
                           let δx = δ x
                           writeArray xs i (x + δx)
                           !v <- eval_all cs <$> unsafeFreezeSTUArray xs
                           writeArray xs i x
                           return (v0, (v - v0) / δx)
