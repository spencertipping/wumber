{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Wumber.Constraint where

import Control.Applicative
import Control.Monad
import Control.Monad.RWS
import qualified Data.Map.Lazy as M
import Data.Foldable
import Data.Maybe
import Text.Printf


-- | For documentation. Variable IDs are always going to be integers, and these
--   integers will always refer to indexes in an array or vector.
type VarID = Int
type N     = Double


-- | A constrained variable, constant, or transformation of one or more such
--   values. You build these up with numeric expressions and then assert
--   equivalence using '===', which emits constraints to the solver.
data CVal = CVar        !VarID !N
          | CConst      !N
          | CLinear     !N !N !CVal
          | CNonlinearU !CVal       !(N -> N)      String
          | CNonlinearB !CVal !CVal !(N -> N -> N) String
          | CNonlinear  ![CVal]     !([N] -> N)    String


instance Show CVal where
  show (CVar i v)             = printf "(var %d %f)" i v
  show (CConst v)             = printf "%f" v
  show (CLinear m b v)        = printf "(%f*(%s) + %f)" m (show v) b
  show (CNonlinear ops _ fn)  = printf "(%s %s)" (show fn) (show ops)
  show (CNonlinearU op _ fn)  = printf "(%s %s)" (show fn) (show op)
  show (CNonlinearB l r _ fn) = printf "(%s %s %s)" (show l) (show fn) (show r)


-- | 'Constrained' is a monad that keeps track of 'CVar' IDs and collects
--   constraint expressions whose values should end up being zero.
type Constrained a = RWS () [Constraint] Int a


data Constraint = CEqual  !CVal !CVal
                | CCostFn !CVal


-- | Rewrite 'CVal's by replacing one or more variables with different
--   expressions. Some 'CEqual' constraints can reduce the number of independent
--   variables before we send anything to the numerical optimizer, which makes
--   optimization considerably faster.
(//) :: CVal -> (VarID -> Maybe CVal) -> CVal

v@(CVar i _)         // m = fromMaybe v (m i)
v@(CConst _)         // m = v
CLinear m' b v       // m = linear m' b (v // m)
CNonlinear ops f fn  // m = CNonlinear (map (// m) ops) f fn
CNonlinearU op f fn  // m = nonlinear_unary  f fn (op // m)
CNonlinearB l r f fn // m = nonlinear_binary f fn (l // m) (r // m)


-- | Apply a linear transformation to a single constrained value. The value
--   becomes a constant if you scale it to zero.
linear :: N -> N -> CVal -> CVal
linear 0 b _                 = CConst  b
linear m b (CConst x)        = CConst  (m * x + b)
linear m b (CLinear m' b' v) = linear  (m * m') (b + b') v
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


-- | Constraint equivalence. The premise is that we can reduce each constraint
--   down to one or more scalar values that describe its out-of-whackness. The
--   solver attempts to set these values to zero.
class CEq a where
  infix 4 =-=; (=-=) :: a -> a -> Constrained ()
  infix 4 <-=; (<-=) :: a -> a -> Constrained ()
  infix 4 >-=; (>-=) :: a -> a -> Constrained ()
  (>-=) = flip (<-=)

instance CEq CVal where
  a =-= b = tell [CEqual a b]
  a <-= b = tell [CCostFn $ CNonlinearU (b - a) (max 0) "max 0"]

instance (Foldable f, CEq a) => CEq (f a) where
  a =-= b = sequence_ $ zipWith (=-=) (toList a) (toList b)
  a <-= b = sequence_ $ zipWith (<-=) (toList a) (toList b)


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
  recip v          = nonlinear_unary safe_recip "recip" v

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


-- | Calculate a reciprocal, but return zero instead of Infinity or NaN. We need
--   this workaround to avoid crashing the GSL solver.
safe_recip :: N -> N
safe_recip 0 = 0
safe_recip x = recip x


-- | Normal 'min' and 'max' don't apply to 'CVal's because there isn't a way for
--   'CVal's to implement 'Ord'. What we can do, however, is defer the
--   orderedness to evaluation time.
--
--   'EventuallyOrd' is weaker than 'Ord' in that it lets you construct a value
--   that represents the 'min' or 'max', but doesn't let you compare values.

class EventuallyOrd a where
  cmin :: a -> a -> a
  cmax :: a -> a -> a

instance Ord a => EventuallyOrd a where
  cmin = min
  cmax = max

instance EventuallyOrd CVal where
  cmin = nonlinear_binary min "min"
  cmax = nonlinear_binary max "max"

instance (Applicative f, EventuallyOrd a) => EventuallyOrd (f a) where
  cmin = liftA2 cmin
  cmax = liftA2 cmax


-- | Create a new constrained variable initialized to the given value.
var :: N -> Constrained CVal
var init = do id <- get
              modify (+ 1)
              return $ CVar id init

-- | A multidimensional variant of 'var'.
vars :: Traversable t => t N -> Constrained (t CVal)
vars = mapM var
