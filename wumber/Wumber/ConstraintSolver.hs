{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Wumber.ConstraintSolver where

import Control.Monad
import Control.Monad.ST
import Control.Monad.RWS
import Data.Array.MArray
import Data.Array.ST
import Data.Array.Unsafe
import Data.Array.Unboxed
import GHC.Float
import Lens.Micro
import Lens.Micro.TH
import Linear.V1
import Linear.V2
import Linear.V3

import Wumber.Constraint


-- | The transitive dependency closure of a given 'CVal'.
deps :: CVal -> [VarID]
deps (CVar i _)        = [i]
deps (CConst _)        = []
deps (CLinear _ _ v)   = deps v
deps (CNonlinear xs _) = concatMap deps xs


eval :: CVal -> Array VarID CE -> CE
eval (CVar i _)         xs = xs ! i
eval (CConst x)         _  = x
eval (CLinear m b v)    xs = let x = eval v xs in m*x + b
eval (CNonlinear ops f) xs = f $! map (flip eval xs) ops


ε :: CE
ε = 1e-8

-- | The numerical partial derivative of ∑vs with respect to the given variable.
partial :: MArray a CE m => [CVal] -> a VarID CE -> VarID -> m CE
partial vs xs i = do
  xs'  <- unsafeFreeze xs
  !v0  <- return $! foldl (\t v -> t + eval v xs') 0 vs
  v    <- readArray xs i
  writeArray xs i (v + ε)
  xs'' <- unsafeFreeze xs   -- Rebind to defeat subexpression caching
  !vg  <- return $! foldl (\t v -> t + eval v xs'') 0 vs
  writeArray xs i v
  return $! (vg - v0) / ε
