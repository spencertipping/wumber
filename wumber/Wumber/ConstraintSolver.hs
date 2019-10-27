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


eval :: Vector N -> CVal -> N
eval xs (CVar i _)            = xs ! i
eval _  (CConst x)            = x
eval xs (CLinear m b v)       = let x = eval xs v in m*x + b
eval xs (CNonlinear ops f _)  = f $ map (eval xs) ops
eval xs (CNonlinearU v f _)   = f (eval xs v)
eval xs (CNonlinearB l r f _) = eval xs l `f` eval xs r


eval_all :: [Constraint] -> Vector N -> N
eval_all cs xs = foldl (\t v -> t + eval xs v) 0 cs


constraints_from :: Constrained a -> (a, [Constraint])
constraints_from m = evalRWS m () 0


solve :: Functor f => N -> Int -> Constrained (f CVal)
      -> (f N, Vector N, [Constraint])
solve ε n m = (eval solution <$> a, solution, cs)
  where (a, cs)     = constraints_from m
        f           = eval_all cs
        vars        = S.unions $ map deps cs
        start       = V.replicate (1 + fst (S.findMax vars)) 0 V.// S.toList vars
        search_size = V.replicate (1 + fst (S.findMax vars)) 1
        solution    = fst $ minimizeV NMSimplex2 (ε/2) n search_size f start
