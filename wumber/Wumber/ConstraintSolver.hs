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
import qualified Data.Set as S
import qualified Data.Vector.Storable as V
import GHC.Float
import Lens.Micro
import Numeric.GSL.Minimization
import Numeric.LinearAlgebra.Data
import qualified Linear.Metric as LM
import Linear.V1
import Linear.V2
import Linear.V3

import Wumber.Constraint


-- | All independent variables used to calculate the given value.
deps :: CVal -> S.Set (VarID, N)
deps (CVar i v)            = S.singleton (i, v)
deps (CConst _)            = S.empty
deps (CLinear _ _ v)       = deps v
deps (CNonlinear xs _ _)   = S.unions (map deps xs)
deps (CNonlinearU v _ _)   = deps v
deps (CNonlinearB l r _ _) = deps l `S.union` deps r


eval :: CVal -> Vector N -> N
eval (CVar i _)            xs = xs ! i
eval (CConst x)            _  = x
eval (CLinear m b v)       xs = let x = eval v xs in m*x + b
eval (CNonlinear ops f _)  xs = f $ map (flip eval xs) ops
eval (CNonlinearU v f _)   xs = f (eval v xs)
eval (CNonlinearB l r f _) xs = eval l xs `f` eval r xs


eval_all :: [Constraint] -> Vector N -> N
eval_all cs xs = foldl (\t v -> t + eval v xs) 0 cs


constraints_from :: Constrained a -> (a, [Constraint])
constraints_from m = evalRWS m () 0


solve :: N -> Int -> Constrained a -> (a, Vector N, [Constraint])
solve ε n m = (a, fst $ minimizeV NMSimplex2 (ε/2) n search_size f start, cs)
  where (a, cs)     = constraints_from m
        f           = eval_all cs
        vars        = S.unions $ map deps cs
        start       = V.replicate (1 + fst (S.findMax vars)) 0 V.// S.toList vars
        search_size = V.replicate (1 + fst (S.findMax vars)) 1
