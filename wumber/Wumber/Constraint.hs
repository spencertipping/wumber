{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Wumber.Constraint where

import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.Unboxed
import GHC.Float
import Lens.Micro
import Lens.Micro.TH


type Solution s = STUArray s Int Double
type CostFn     = ([Int], [Double] -> Double)
data Constraint = HardConstant Int Double
                | HardOffset   Int Int Double
                | Soft         CostFn


-- | Describes a set of constraints after we've propagated constants and
--   offsets. '_cs_constants' maps each entry either to a finite Double value,
--   or to NaN if the entry is variable.
data ConstraintSet = CSet { _cs_constants :: UArray Int Double,
                            _cs_fn        :: forall s. Solution s -> ST s Double,
                            _cs_fns       :: Array Int [CostFn] }

makeLenses ''ConstraintSet


nan :: Double
nan = 0/0

ε :: Double
ε = 1e-8


constant_fold :: [Constraint] -> ConstraintSet
constant_fold cs = CSet consts eval undefined
  where eval s                         = sum <$> mapM (apply_cost_fn s) fns
        apply_cost_fn s (Soft (is, f)) = f   <$> mapM (readArray s) is
        fns                            = filter (\case Soft _ -> True
                                                       _      -> False) cs

        var_set_ubound cs = foldl1 max $ map single_idx cs
          where single_idx (HardConstant i _) = i
                single_idx (HardOffset i j _) = i `max` j
                single_idx (Soft (is, _))     = foldl1 max is

        consts = runSTUArray do
          vs <- newArray (0, var_set_ubound cs) nan
          forM_ cs \case HardConstant i v -> writeArray vs i v
                         _                -> return ()

          forM_ cs \case HardOffset i j o -> do
                           v <- readArray vs j
                           unless (isNaN v) $ writeArray vs i (v + o)
                         _                -> return ()
          return vs
