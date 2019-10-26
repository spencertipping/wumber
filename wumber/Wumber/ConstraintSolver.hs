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
import Linear.V1
import Linear.V2
import Linear.V3

import Wumber.Constraint

import Debug.Trace


-- | A solved system whose values are available using 'evalM'.
type Solved a = Reader (UArray VarID CE) a


-- | All independent variables used to calculate the given value.
deps :: CVal -> Set (VarID, CE)
deps (CVar i v)        = singleton (i, v)
deps (CConst _)        = empty
deps (CLinear _ _ v)   = deps v
deps (CNonlinear xs _) = unions (map deps xs)


eval :: CVal -> UArray VarID CE -> CE
eval (CVar i _)         xs = xs ! i
eval (CConst x)         _  = x
eval (CLinear m b v)    xs = let !x = eval v xs in m*x + b
eval (CNonlinear ops f) xs = f $! map (flip eval xs) ops


evalM :: CVal -> Solved CE
evalM v = eval v <$> ask


ε :: CE
ε = 1e-8

nan :: CE
nan = 0/0

-- | Calculates the value and the partial derivative of the sum of specified
--   functions at the variable in question.
{-# SPECIALIZE
    partial :: [Constraint] -> (STUArray s) VarID CE -> VarID -> ST s (CE, CE) #-}
partial :: MArray a CE m => [Constraint] -> a VarID CE -> VarID -> m (CE, CE)
partial cs xs i = do
  xs'  <- unsafeFreeze xs
  !v0  <- return $! foldl (\t v -> t + eval v xs') 0 cs
  !v   <- readArray xs i
  writeArray xs i (v + ε)
  xs'' <- unsafeFreeze xs   -- Rebind to defeat subexpression caching
  !vg  <- return $! foldl (\t v -> t + eval v xs'') 0 cs
  writeArray xs i v
  return (v0, (vg - v0) / ε)


-- | Steps Newton's method by one iteration.
newton_step :: MArray a CE (ST s)
            => (a VarID CE -> VarID -> ST s (CE, CE))
            -> [VarID] -> a VarID CE -> a VarID CE -> a VarID CE -> ST s CE
newton_step partial var_ids xs ps ss = do
  t <- newSTRef 0
  forM_ var_ids \i -> do (v, g) <- partial xs i
                         writeArray ps i (v * signum g / max ε g)
                         modifySTRef t (+ v)
  forM_ var_ids \i -> do x <- readArray xs i
                         d <- readArray ps i
                         s <- readArray ss i
                         if signum s /= signum d
                           then writeArray ss i (s * (-0.5))
                           else writeArray ss i (s * 1.01)
                         writeArray xs i $ trace ("(x, d, s) = " ++ show (x, d, s)) (x - d * abs s)
  readSTRef t

newton_solve :: MArray a CE (ST s)
             => Int
             -> (a VarID CE -> VarID -> ST s (CE, CE))
             -> [VarID] -> a VarID CE -> a VarID CE -> a VarID CE -> ST s CE
newton_solve n partial var_ids xs ps ss = do
  v <- newton_step partial var_ids xs ps ss
  if trace (show v) v <= ε || n <= 0
    then return v
    else newton_solve (n - 1) partial var_ids xs ps ss


-- | Collects constraints, sets initial values, and solves a system using
--   gradient descent. All provided 'CVal's will be minimized, although perhaps
--   not zero.
solve :: Constrained a -> (CE, UArray VarID CE)
solve m = runST do
  xs <- newArray (0, vmax) 0
  ps <- newArray (0, vmax) 0
  ss <- newArray (0, vmax) 1

  forM_ vars \(i, v) -> writeArray xs i v
  r   <- newton_solve 10000 (partial cs) var_ids xs ps ss
  xs' <- unsafeFreezeSTUArray xs
  return (r, xs')

  where cs      = snd $ evalRWS m () 0
        vars    = unions (map deps cs)
        vmax    = fst $ S.findMax vars
        var_ids = map fst $ S.toList vars
