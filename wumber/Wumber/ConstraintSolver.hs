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


-- TODO
-- We shouldn't use a global epsilon; for cases like 'partial' we should
-- parameterize it on the current values. Epsilon should provided a fixed number
-- of mantissa bits of displacement, not a fixed real-world magnitude.
ε :: N
ε = 1e-8


-- | Calculates the value and the partial derivative of the sum of specified
--   functions at the variable in question.
{-# SPECIALIZE INLINE
    partial :: [Constraint] -> (STUArray s) VarID N -> VarID -> ST s (N, N) #-}
partial :: MArray a N (ST s) => [Constraint] -> a VarID N -> VarID -> ST s (N, N)
partial cs xs i = do
  xs'  <- unsafeFreeze xs
  !v0  <- return $! foldl (\t v -> t + eval v xs') 0 cs
  !v   <- readArray xs i
  writeArray xs i (v + ε)
  xs'' <- unsafeFreeze xs   -- Rebind to defeat subexpression caching
  !vg  <- return $! foldl (\t v -> t + eval v xs'') 0 cs
  writeArray xs i v
  return (v0, (vg - v0) / ε)


-- | Adjusts the estimate used in Newton's method. This handles some edge cases
--   and limits the step size when the gradient is very small.
--
--   If the gradient is zero, we adjust by a small amount: √ε. This won't move
--   us closer to a solution, but it might get us past the plateau.

{-# INLINE newton_adjust #-}
newton_adjust :: N -> N -> N -> (N, N)
newton_adjust v g s
  | g == 0            = (signum s' * sqrt ε, s')
  | abs g < max_slope = (abs s' * v/max_slope * signum g, s')
  | otherwise         = (abs s' * v/g, s')

  where max_slope = sqrt ε
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
          let (x', s') = newton_adjust v g s
          writeArray ps i x'
          writeArray ss i s'
          modifySTRef t (+ v)

        update_variables = forM_ var_ids \i -> do
          !x <- readArray xs i
          !d <- readArray ps i
          writeArray xs i $! x - d


-- | Solves a system using up to 'n' iterations.
newton_solve n partial var_ids xs ps ss = do
  v <- newton_step partial var_ids xs ps ss
  if v <= ε || n <= 0
    then return (v, n)
    else newton_solve (n - 1) partial var_ids xs ps ss


-- | Collects constraints, sets initial values, and solves a system using
--   Newton's method.
--
--   Linear constraints are easily solvable unless the system is overspecified:
--
--   prop>       solvable 100 $ do v <- var x; v =-= CConst y
--   prop> not $ solvable 100 $ do v <- var x; v =-= CConst y; v =-= CConst (y+1)
--
--   'solve' also handles nonlinear constraints, but initial values matter
--   because we're using Newton's method.

solve :: Int -> Constrained a -> (N, Int, UArray VarID N)
solve n m = runST do
  xs <- newArray (0, vmax) 0  :: ST s (STUArray s VarID N)
  ps <- newArray (0, vmax) 0  :: ST s (STUArray s VarID N)
  ss <- newArray (0, vmax) s0 :: ST s (STUArray s VarID N)
  forM_ vars \(i, v) -> writeArray xs i v

  (r, n') <- newton_solve n (partial cs) var_ids xs ps ss
  xs'     <- unsafeFreezeSTUArray xs
  return (r, n', xs')

  where cs      = snd $ evalRWS m () 0
        vars    = unions (map deps cs)
        vmax    = fst $ S.findMax vars
        var_ids = map fst $ S.toList vars
        s0      = 0.5 / sqrt (fromIntegral $ length var_ids)


-- | For testing: a system is solvable iff it converges to error below the
--   epsilon and hasn't exhausted its iteration count.
solvable :: Int -> Constrained a -> Bool
solvable n m = v <= ε && n' > 0 where (v, n', _) = solve n m


testcase1 = do
  v1 <- vars (V3 1 1 1)
  v2 <- vars (V2 1 1)
  LM.norm v1 =-= 1
  LM.norm v2 =-= 1


testcase2 = do
  v1 <- vars (V2 1 1)
  v2 <- vars (V2 2 3)
  LM.distance v1 v2 =-= 4
  v1^._x =-= v1^._y
  v2^._x =-= v2^._y
