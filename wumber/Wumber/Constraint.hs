{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Wumber.Constraint where

import Control.Monad
import Control.Monad.ST
import Control.Monad.RWS
import Data.Array.ST
import Data.Array.Unboxed
import GHC.Float
import Lens.Micro
import Lens.Micro.TH


ε :: Double
ε = 1e-8


-- | A constrained variable, constant, or a linear transformation of a
--   constrained thing.
data CVal = CVar    Int Double
          | CConst  Double
          | CLinear { _cl_m :: Double, _cl_b :: Double, _cl_v :: CVal }
  deriving (Show, Read, Eq, Ord)

type Constraint    = ([CVal], [Double] -> Double)
type Constrained a = forall s. RWST () [Constraint] Int (ST s) a


-- | Apply a linear transformation to a single constrained value.
linear :: Double -> Double -> CVal -> CVal
linear m b (CConst x)        = CConst  (m * x + b)
linear m b (CLinear m' b' v) = CLinear (m * m') (b + b') v
linear m b v                 = CLinear m b v


instance Num CVal where
  fromInteger  = CConst . fromInteger
  negate       = linear (-1) 0
  CConst x + y = linear 1 x y
  x + CConst y = linear 1 y x
  CConst x * y = linear x 0 y
  x * CConst y = linear y 0 x

  abs (CConst x)    = CConst (abs x)
  abs v             = error ("abs(" ++ show v ++ ") is nonlinear")
  signum (CConst x) = CConst (signum x)
  signum v          = error ("signum(" ++ show v ++ ") is nonlinear")

instance Fractional CVal where
  fromRational = CConst . fromRational
  recip (CConst x) = CConst (recip x)
  recip v          = error ("recip(" ++ show v ++ ") is nonlinear")


-- | Create a new constrained variable initialized to the given value.
var :: Double -> Constrained CVal
var init = do
  id <- get
  modify (+ 1)
  return $ CVar id init


