{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Wumber.Constraint where

import Control.Monad
import Control.Monad.ST
import Control.Monad.RWS
import Data.Array.MArray
import Data.Array.ST
import Data.Array.Unboxed
import GHC.Float
import Lens.Micro
import Lens.Micro.TH
import Linear.V1
import Linear.V2
import Linear.V3


type CE    = Double
type VarID = Int


-- | A constrained variable, constant, or transformation of one or more such
--   values. You build these up with numeric expressions and then assert
--   equivalence using '===', which emits constraints to the solver.
data CVal = CVar       !VarID !CE
          | CConst     !CE
          | CLinear    { _cl_m :: !CE, _cl_b :: !CE, _cl_v :: !CVal }
          | CNonlinear { _cln_operands :: ![CVal],
                         _cln_fn       :: !([CE] -> CE) }
makeLenses ''CVal


-- | 'Constrained' is a monad that keeps track of 'CVar' IDs and collects
--   constraint expressions whose values should end up being zero. Constraints
--   are solved in the 'ST' monad using mutable unboxed 'Double' arrays.
type Constrained a = RWS () [Constraint] Int a

-- | Constraints are just values that we want to set to zero.
type Constraint = CVal


-- | Constraint equivalence. The premise is that we can reduce each constraint
--   down to one or more scalar values that describe its out-of-whackness. The
--   solver attempts to set these values to zero.
infix 4 =:=
class CEq a where (=:=) :: a -> a -> Constrained ()
instance CEq CVal            where a        =:= b        = tell [abs $ a - b]
instance CEq a => CEq (V1 a) where V1 a     =:= V1 b     = do a =:= b
instance CEq a => CEq (V2 a) where V2 a b   =:= V2 c d   = do a =:= c; b =:= d
instance CEq a => CEq (V3 a) where V3 a b c =:= V3 d e f = do a =:= d; b =:= e; c =:= f


-- | Boolean 'and' (intersection) of a set of constraints.
cand :: [Constraint] -> Constraint
cand xs = CNonlinear xs (foldl1 min)

-- | Boolean 'or' (union) of a set of constraints.
cor :: [Constraint] -> Constraint
cor xs = CNonlinear xs (foldl1 max)


-- | Apply a linear transformation to a single constrained value. The value
--   becomes a constant if you scale it to zero.
linear :: CE -> CE -> CVal -> CVal
linear 0 b _                 = CConst  b
linear m b (CConst x)        = CConst  (m * x + b)
linear m b (CLinear m' b' v) = CLinear (m * m') (b + b') v
linear m b v                 = CLinear m b v

-- | Promote a unary function to a nonlinear transformation, with constant
--   folding.
nonlinear_unary :: (CE -> CE) -> CVal -> CVal
nonlinear_unary f (CConst x) = CConst (f x)
nonlinear_unary f v = CNonlinear [v] (f . head)

-- | Promote a binary function to a nonlinear transformation, with constant
--   folding.
nonlinear_binary :: (CE -> CE -> CE) -> CVal -> CVal -> CVal
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
vars :: Traversable t => t CE -> Constrained (t CVal)
vars = mapM var

var :: CE -> Constrained CVal
var init = do id <- get
              modify (+ 1)
              return $ CVar id init
