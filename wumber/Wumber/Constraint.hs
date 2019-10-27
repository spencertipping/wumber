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
import Text.Printf


type N     = Double
type VarID = Int


-- | A constrained variable, constant, or transformation of one or more such
--   values. You build these up with numeric expressions and then assert
--   equivalence using '===', which emits constraints to the solver.
data CVal = CVar        !VarID !N
          | CConst      !N
          | CLinear     { _cl_m :: !N, _cl_b :: !N, _cl_v :: !CVal }
          | CNonlinearU { _clnu_op      :: !CVal,
                          _clnu_fn      :: !(N -> N),
                          _clnu_fname   :: String }
          | CNonlinearB { _clnb_lhs     :: !CVal,
                          _clnb_rhs     :: !CVal,
                          _clnb_fn      :: !(N -> N -> N),
                          _clnb_fname   :: String }
          | CNonlinear  { _cln_operands :: ![CVal],
                          _cln_fn       :: !([N] -> N),
                          _cln_fname    :: String}

makeLenses ''CVal


instance Show CVal where
  show (CVar i v)             = printf "(var %d %f)" i v
  show (CConst v)             = printf "%f" v
  show (CLinear m b v)        = printf "(%f*(%s) + %f)" m (show v) b
  show (CNonlinear ops _ fn)  = printf "(%s %s)" (show fn) (show ops)
  show (CNonlinearU op _ fn)  = printf "(%s %s)" (show fn) (show op)
  show (CNonlinearB l r _ fn) = printf "(%s %s %s)" (show l) (show fn) (show r)


-- | 'Constrained' is a monad that keeps track of 'CVar' IDs and collects
--   constraint expressions whose values should end up being zero. Constraints
--   are solved in the 'ST' monad using mutable unboxed 'Double' arrays.
type Constrained a = RWS () [Constraint] Int a

-- | Constraints are just values that we want to set to zero.
type Constraint = CVal


-- | Constraint equivalence. The premise is that we can reduce each constraint
--   down to one or more scalar values that describe its out-of-whackness. The
--   solver attempts to set these values to zero.
class CEq a where (=-=) :: a -> a -> Constrained ()
                  (<-=) :: a -> a -> Constrained ()
                  (>-=) :: a -> a -> Constrained ()
                  (>-=) = flip (<-=)
infix 4 =-=
infix 4 <-=
infix 4 >-=

instance CEq CVal where
  a =-= b = tell [abs $ a - b]
  a <-= b = tell [CNonlinearU (a - b) (max 0) "max 0"]

instance CEq a => CEq (V1 a) where
  V1 a =-= V1 b = do a =-= b
  V1 a <-= V1 b = do a <-= b

instance CEq a => CEq (V2 a) where
  V2 a b =-= V2 c d = do a =-= c; b =-= d
  V2 a b <-= V2 c d = do a <-= c; b <-= d

instance CEq a => CEq (V3 a) where
  V3 a b c =-= V3 d e f = do a =-= d; b =-= e; c =-= f
  V3 a b c <-= V3 d e f = do a <-= d; b <-= e; c <-= f


-- | Boolean 'and' (intersection) of a set of constraints.
cand :: [Constraint] -> Constraint
cand xs = CNonlinear xs (foldl1 min) "cand"

-- | Boolean 'or' (union) of a set of constraints.
cor :: [Constraint] -> Constraint
cor xs = CNonlinear xs (foldl1 max) "cor"


-- | Apply a linear transformation to a single constrained value. The value
--   becomes a constant if you scale it to zero.
linear :: N -> N -> CVal -> CVal
linear 0 b _                 = CConst  b
linear m b (CConst x)        = CConst  (m * x + b)
linear m b (CLinear m' b' v) = CLinear (m * m') (b + b') v
linear m b v                 = CLinear m b v

-- | Promote a unary function to a nonlinear transformation, with constant
--   folding.
nonlinear_unary :: (N -> N) -> String -> CVal -> CVal
nonlinear_unary f fname (CConst x) = CConst (f x)
nonlinear_unary f fname v = CNonlinearU v f fname

-- | Promote a binary function to a nonlinear transformation, with constant
--   folding.
nonlinear_binary :: (N -> N -> N) -> String -> CVal -> CVal -> CVal
nonlinear_binary f fname (CConst x) (CConst y) = CConst (f x y)
nonlinear_binary f fname x y = CNonlinearB x y f fname


instance Num CVal where
  fromInteger  = CConst . fromInteger
  negate       = linear (-1) 0
  CConst x + y = linear 1 x y
  x + CConst y = linear 1 y x
  x + y        = nonlinear_binary (+) "+" x y
  CConst x * y = linear x 0 y
  x * CConst y = linear y 0 x
  x * y        = nonlinear_binary (*) "*" x y

  abs (CConst x)    = CConst (abs x)
  abs v             = nonlinear_unary abs "abs" v
  signum (CConst x) = CConst (signum x)
  signum v          = nonlinear_unary signum "signum" v

instance Fractional CVal where
  fromRational     = CConst . fromRational
  recip (CConst x) = CConst (recip x)
  recip v          = nonlinear_unary recip "recip" v

instance Floating CVal where
  pi    = CConst pi
  (**)  = nonlinear_binary (**) "**"
  sqrt  = nonlinear_unary sqrt "sqrt"
  exp   = nonlinear_unary exp "exp"
  log   = nonlinear_unary log "log"
  sin   = nonlinear_unary sin "sin"
  cos   = nonlinear_unary cos "cos"
  asin  = nonlinear_unary asin "asin"
  acos  = nonlinear_unary acos "acos"
  atan  = nonlinear_unary atan "atan"
  sinh  = nonlinear_unary sinh "sinh"
  cosh  = nonlinear_unary cosh "cosh"
  asinh = nonlinear_unary asinh "asinh"
  acosh = nonlinear_unary acosh "acosh"
  atanh = nonlinear_unary atanh "atanh"


-- | Create a new constrained variable initialized to the given value.
var :: N -> Constrained CVal
var init = do id <- get
              modify (+ 1)
              return $ CVar id init

vars :: Traversable t => t N -> Constrained (t CVal)
vars = mapM var
