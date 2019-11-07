{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}


module Wumber.Symbolic (
  Math(..)
) where


import Control.Applicative
import Control.Monad
import Control.Monad.RWS
import Data.Foldable
import Data.Maybe
import Text.Printf (printf)

import Wumber.ClosedComparable


-- | Symbolic math operations, plus 'max' and 'min'.
data Math a = Num    a

            | Plus   (Math a) (Math a)
            | Negate (Math a)
            | Times  (Math a) (Math a)
            | Recip  (Math a)
            | Abs    (Math a)
            | Signum (Math a)

            | Max    (Math a) (Math a)
            | Min    (Math a) (Math a)

            | Pow    (Math a) (Math a)
            | Sqrt   (Math a)
            | Exp    (Math a)
            | Log    (Math a)
            | Sin    (Math a)
            | Cos    (Math a)
            | Tan    (Math a)
            | Asin   (Math a)
            | Acos   (Math a)
            | Atan   (Math a)
            | Sinh   (Math a)
            | Cosh   (Math a)
            | Tanh   (Math a)
            | Asinh  (Math a)
            | Acosh  (Math a)
            | Atanh  (Math a)

  deriving (Eq, Ord, Functor, Foldable, Traversable)


eval :: (Floating n, ClosedComparable n) => (a -> n) -> Math a -> n
eval f (Num a)     = f a
eval f (Plus a b)  = eval f a + eval f b
eval f (Negate a)  = negate $ eval f a
eval f (Times a b) = eval f a * eval f b
eval f (Recip a)   = recip $ eval f a
eval f (Abs a)     = abs $ eval f a
eval f (Signum a)  = signum $ eval f a
eval f (Max a b)   = eval f a `upper` eval f b
eval f (Min a b)   = eval f a `lower` eval f b
eval f (Pow a b)   = eval f a ** eval f b
eval f (Sqrt a)    = sqrt $ eval f a
eval f (Exp a)     = exp $ eval f a
eval f (Log a)     = log $ eval f a
eval f (Sin a)     = sin $ eval f a
eval f (Cos a)     = cos $ eval f a
eval f (Tan a)     = tan $ eval f a
eval f (Asin a)    = asin $ eval f a
eval f (Acos a)    = acos $ eval f a
eval f (Atan a)    = atan $ eval f a
eval f (Sinh a)    = sinh $ eval f a
eval f (Cosh a)    = cosh $ eval f a
eval f (Tanh a)    = tanh $ eval f a
eval f (Asinh a)   = asinh $ eval f a
eval f (Acosh a)   = acosh $ eval f a
eval f (Atanh a)   = atanh $ eval f a


instance Show a => Show (Math a) where
  show (Num a) = show a

  show (Plus a (Negate b)) = printf "(%s - %s)" (show a) (show b)
  show (Plus a b)          = printf "(%s + %s)" (show a) (show b)
  show (Negate a)          = printf "(-%s)" (show a)

  show (Times a (Recip b)) = printf "(%s / %s)" (show a) (show b)
  show (Times a b)         = printf "(%s * %s)" (show a) (show b)
  show (Recip a)           = printf "(%s⁻¹)" (show a)

  show (Abs a)    = printf "|%s|" (show a)
  show (Signum a) = printf "sgn(%s)" (show a)
  show (Max a b)  = printf "(%s max %s)" (show a) (show b)
  show (Min a b)  = printf "(%s min %s)" (show a) (show b)

  show (Pow a b) = printf "(%s ** %s)" (show a) (show b)
  show (Sqrt a)  = printf "√(%s)" (show a)
  show (Exp a)   = printf "exp(%s)" (show a)
  show (Log a)   = printf "log(%s)" (show a)
  show (Sin a)   = printf "sin(%s)" (show a)
  show (Cos a)   = printf "cos(%s)" (show a)
  show (Tan a)   = printf "tan(%s)" (show a)
  show (Asin a)  = printf "sin⁻¹(%s)" (show a)
  show (Acos a)  = printf "cos⁻¹(%s)" (show a)
  show (Atan a)  = printf "tan⁻¹(%s)" (show a)
  show (Sinh a)  = printf "sinh(%s)" (show a)
  show (Cosh a)  = printf "cosh(%s)" (show a)
  show (Tanh a)  = printf "tanh(%s)" (show a)
  show (Asinh a) = printf "sinh⁻¹(%s)" (show a)
  show (Acosh a) = printf "cosh⁻¹(%s)" (show a)
  show (Atanh a) = printf "tanh⁻¹(%s)" (show a)


instance Num a => Num (Math a) where
  fromInteger = Num . fromInteger
  (+)         = Plus
  negate      = Negate
  (*)         = Times
  abs         = Abs
  signum      = Signum

instance Fractional a => Fractional (Math a) where
  fromRational = Num . fromRational
  recip        = Recip

instance Floating a => Floating (Math a) where
  pi    = Num pi
  exp   = Exp
  log   = Log
  sqrt  = Sqrt
  (**)  = Pow
  sin   = Sin
  cos   = Cos
  tan   = Tan
  asin  = Asin
  acos  = Acos
  atan  = Atan
  sinh  = Sinh
  cosh  = Cosh
  tanh  = Tanh
  asinh = Asinh
  acosh = Acosh
  atanh = Atanh

instance ClosedComparable (Math a) where
  lower = Min
  upper = Max


{-

-- | For documentation. Variable IDs are always going to be integers, and these
--   integers will always refer to indexes in an array or vector.
type VarID = Int
type N     = Double

-- TODO: we'll be more JIT-able if we go with a typeclass or 'data' with
-- symbolic functions. Then we get name/compile/etc functions, and we might also
-- be in a better position to solve equations without relying on 'CLinear'.
type FName = String


-- | A constrained variable, constant, or transformation of one or more such
--   values. You build these up with numeric expressions and then assert
--   equivalence using methods in 'CEq', which emit constraints to the solver.

-- TODO: add CSum or some such; we want multi-term quantities to remain symbolic
-- so we can reduce dimensionality via matrix inversion in the constraint
-- simplifier.

-- TODO: dependencies of CVal, where deps can come out tagged with linear
-- metadata, e.g. "this CVal depends linearly on variable 5: slope is 8, offset
-- is 10".

data CVal = CVar        !VarID !N
          | CConst      !N
          | CLinear     !N !N !CVal
          | CNonlinearU !CVal       !(N -> N)      FName
          | CNonlinearB !CVal !CVal !(N -> N -> N) FName
          | CNonlinear  ![CVal]     !([N] -> N)    FName


instance Show CVal where
  show (CVar i v)             = printf "(var %d %f)" i v
  show (CConst v)             = printf "%f" v
  show (CLinear m b v)        = printf "(%f*(%s) + %f)" m (show v) b
  show (CNonlinear ops _ fn)  = printf "(%s %s)" (show fn) (show ops)
  show (CNonlinearU op _ fn)  = printf "(%s %s)" (show fn) (show op)
  show (CNonlinearB l r _ fn) = printf "(%s %s %s)" (show l) (show fn) (show r)

instance {-# OVERLAPPABLE #-} ClosedComparable CVal where
  lower = nonlinear_binary min "min"
  upper = nonlinear_binary max "max"


-- | 'Constrained' is a monad that keeps track of 'CVar' IDs and collects
--   constraint expressions whose values should end up being zero.
type Constrained a = RWS () [Constraint] Int a


-- | Create a new constrained variable initialized to the given value.
var :: N -> Constrained CVal
var init = do id <- get
              modify (+ 1)
              return $ CVar id init

-- | A multidimensional variant of 'var'.
vars :: Traversable t => t N -> Constrained (t CVal)
vars = mapM var


-- | Constraints to be solved or minimized. Although both can be reduced to cost
--   functions, you should use 'CEqual' when possible because the solver can
--   often do some algebra up front to simplify equation systems before the
--   numerical step.
data Constraint = CEqual    !CVal !CVal
                | CMinimize !CVal


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
nonlinear_unary :: (N -> N) -> FName -> CVal -> CVal
nonlinear_unary f fname v = case v of
  CConst x              -> CConst (f x)
  CNonlinearU v' f' fn' -> nonlinear_unary (f . f') (fname ++ " . " ++ fn') v'
  _                     -> CNonlinearU v f fname

-- | Promote a binary function to a nonlinear transformation, with constant
--   folding.
nonlinear_binary :: (N -> N -> N) -> FName -> CVal -> CVal -> CVal
nonlinear_binary f fname x y = case (x, y) of
  (CConst x', CConst y') -> CConst (f x' y')
  (CConst x', _) -> nonlinear_unary (     f x') (fname ++   " " ++ show x') y
  (_, CConst y') -> nonlinear_unary (flip f y') (fname ++ " _ " ++ show y') x
  _              -> CNonlinearB x y f fname


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
  a <-= b = tell [CMinimize $ (b - a) `upper` CConst 0]

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

  abs v        = nonlinear_unary abs "abs" v
  signum v     = nonlinear_unary signum "signum" v

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
-}
