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
import Data.Set (Set, singleton, empty, unions)
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
deps :: CVal -> Set (VarID, N)
deps (CVar i v)            = singleton (i, v)
deps (CConst _)            = empty
deps (CLinear _ _ v)       = deps v
deps (CNonlinear xs _ _)   = unions (map deps xs)
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


-- | Produces a delta sized appropriately to the number in question: that is,
--   halfway into the mantissa. We want parameterized deltas for numerical
--   stability.
δ :: Double -> Double
δ x = x * 2**(-26)


{-|
Solves a system of constraints (or minimizes the error) and returns a tuple of
results:

> (error, remaining_iterations, solution)

We use a modified multivariate Newton's method that finds the gradient by
bisecting the partial derivatives for zero crossings.
-}
solve :: Int -> N -> Constrained a -> (N, Int, UArray VarID N)
solve n ε m = (0, 0, listArray (0, 0) [0])
  where cs      = snd $ evalRWS m () 0
        vars    = unions (map deps cs)
        vmax    = fst $ S.findMax vars
        var_ids = map fst $ S.toList vars
        s0      = 0.5 / fromIntegral (length var_ids)


csum :: [Constraint] -> UArray VarID N -> N
csum cs xs = foldl (\t v -> t + eval v xs) 0 cs


-- | Calculates the partial derivative of the given axis at the given point.
partial :: N -> [Constraint] -> STUArray s VarID N -> VarID -> ST s N
partial v0 cs xs i = do
  x <- readArray xs i
  writeArray xs i (x + δ x)
  xs' <- unsafeFreezeSTUArray xs
  !v  <- return $! csum cs xs'
  writeArray xs i x
  return $ (v - v0) / δ x


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
solvable n ε m = v <= ε && n' > 0 where (v, n', _) = solve n ε m
