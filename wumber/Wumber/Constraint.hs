{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Wumber.Constraint where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
import GHC.Float


-- TODO
-- Is there any merit to having zero-crossings be a thing in constraints?
-- Implicits use negative values, and constraints seem like the type of thing
-- that might benefit from something similar.
--
-- This gives us a way to specify implicit constraint ranges, which may be of
-- some use.

type Solution   = forall s. STUArray s Int Double
data Constraint = HardConstant Int Double
                | HardOffset   Int Int Double
                | SoftCostFn   [Int] ([Double] -> Double)


-- | Describes a set of constraints after we've propagated constants and
--   offsets. '_cs_constants' maps each entry either to a finite Double value,
--   or to NaN if the entry is variable.
data ConstraintSet = CSet { _cs_constants :: UArray Int Double,
                            _cs_fn        :: UArray Int Double -> Double }


nan :: Double
nan = 0/0

ε :: Double
ε = 1e-8


var_set_ubound :: [Constraint] -> Int
var_set_ubound cs = foldl1 max (map single_idx cs)
  where single_idx (HardConstant i _) = i
        single_idx (HardOffset i j _) = i `max` j
        single_idx (SoftCostFn is _)  = foldl1 max is

apply_cost_fn :: UArray Int Double -> Constraint -> Double
apply_cost_fn s (SoftCostFn is f) = f $ map (s !) is
apply_cost_fn _ _ = error "apply_cost_fn on a hard constraint"

constant_fold :: [Constraint] -> ConstraintSet
constant_fold cs = CSet consts eval
  where fns    = filter (\case SoftCostFn _ _ -> True
                               _              -> False) cs
        eval s = sum $ map (apply_cost_fn s) fns
        consts = runSTUArray do
          vs <- newArray (0, var_set_ubound cs) nan
          forM_ cs \case HardConstant i v -> writeArray vs i v
                         _                -> return ()

          forM_ cs \case HardOffset i j o -> do
                           v <- readArray vs j
                           unless (isNaN v) $ writeArray vs i (v + o)
                         _                -> return ()
          return vs
