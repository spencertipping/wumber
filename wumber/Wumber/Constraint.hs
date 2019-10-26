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
data CVal = CVar    !Int !Double
          | CConst  !Double
          | CLinear { _cl_m :: !Double, _cl_b :: !Double, _cl_v :: !CVal }
          | CNonlinear { _cln_operands :: ![CVal],
                         _cln_fn       :: !([Double] -> Double) }
makeLenses ''CVal

-- | 'Constrained' is a monad that keeps track of 'CVar' IDs and collects
--   constraint expressions whose values should end up being zero. Constraints
--   are solved in the 'ST' monad using mutable unboxed 'Double' arrays.
--
--   TODO: do we need RWST here, or is this RWS and then post-ST?
type Constrained a = forall s. RWST () [CVal] Int (ST s) a


-- | Apply a linear transformation to a single constrained value.
linear :: Double -> Double -> CVal -> CVal
linear m b (CConst x)        = CConst  (m * x + b)
linear m b (CLinear m' b' v) = CLinear (m * m') (b + b') v
linear m b v                 = CLinear m b v

-- | Promote a unary function to a nonlinear transformation, with constant
--   folding.
nonlinear_unary :: (Double -> Double) -> CVal -> CVal
nonlinear_unary f (CConst x) = CConst (f x)
nonlinear_unary f v = CNonlinear [v] (f . head)

-- | Promote a binary function to a nonlinear transformation, with constant
--   folding.
nonlinear_binary :: (Double -> Double -> Double) -> CVal -> CVal -> CVal
nonlinear_binary f (CConst x) (CConst y) = CConst (f x y)
nonlinear_binary f x y = CNonlinear [x, y] (\[a, b] -> f a b)


instance Num CVal where
  fromInteger  = CConst . fromInteger
  negate       = linear (-1) 0
  CConst x + y = linear 1 x y
  x + CConst y = linear 1 y x
  x + y        = nonlinear_binary (+) x y
  CConst x * y = linear x 0 y
  x * CConst y = linear y 0 x
  x * y        = nonlinear_binary (*) x y

  abs (CConst x)    = CConst (abs x)
  abs v             = nonlinear_unary abs v
  signum (CConst x) = CConst (signum x)
  signum v          = nonlinear_unary signum v

instance Fractional CVal where
  fromRational = CConst . fromRational
  recip (CConst x) = CConst (recip x)
  recip v          = nonlinear_unary recip v

instance Floating CVal where
  pi    = CConst pi
  exp   = nonlinear_unary exp
  log   = nonlinear_unary log
  sin   = nonlinear_unary sin
  cos   = nonlinear_unary cos
  asin  = nonlinear_unary asin
  acos  = nonlinear_unary acos
  atan  = nonlinear_unary atan
  sinh  = nonlinear_unary sinh
  cosh  = nonlinear_unary cosh
  asinh = nonlinear_unary asinh
  acosh = nonlinear_unary acosh
  atanh = nonlinear_unary atanh


-- | Create a new constrained variable initialized to the given value.
var :: Double -> Constrained CVal
var init = do
  id <- get
  modify (+ 1)
  return $ CVar id init


eval :: forall s. CVal -> STUArray s Int Double -> ST s Double
eval (CVar i _)         xs = readArray xs i
eval (CConst x)         _  = return x
eval (CLinear m b v)    xs = do x <- eval v xs; return $ m*x + b
eval (CNonlinear ops f) xs = f <$> mapM (flip eval xs) ops


-- | Specifies that two constrained expressions should have the same value. We
--   assert this by creating a zero crossing when they are equal -- i.e. we
--   subtract them.
(===) :: CVal -> CVal -> Constrained ()
a === b = tell [a - b]
