{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}


module Wumber.Symbolic (
  Math(..),
  eval
) where


import Text.Printf (printf)

import Wumber.ClosedComparable


-- | Symbolic math operations, plus upper/lower. 'N' is a backdoor into an
--   arbitrary type that you specify. 'a' should be 'Constable'; if operands are
--   'is_const' then the operations will happen at construction time and won't
--   be present in the symbolic value.
data Math a = N a

            | Math a :+ Math a
            | Math a :- Math a
            | Math a :* Math a
            | Math a :/ Math a

            | Abs    (Math a)
            | Signum (Math a)
            | Upper  (Math a) (Math a)
            | Lower  (Math a) (Math a)

            | Math a :** Math a
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

  deriving (Eq, Functor, Foldable, Traversable)

infixl 6 :+
infixl 6 :-
infixl 7 :*
infixl 7 :/
infixl 8 :**


-- | Values that can tell you whether they are constants -- i.e. whether 'Math'
--   should try to collapse them at construction-time.
class Constable a where is_const :: a -> Bool

instance Constable a => Constable (Math a) where
  is_const (N a) = is_const a
  is_const _     = False

instance Constable Double where is_const _ = True
instance Constable Float  where is_const _ = True


instance Show a => Show (Math a) where
  show (N a) = show a

  show (a :+ b) = printf "(%s + %s)" (show a) (show b)
  show (a :- b) = printf "(%s - %s)" (show a) (show b)
  show (a :* b) = printf "(%s * %s)" (show a) (show b)
  show (a :/ b) = printf "(%s / %s)" (show a) (show b)

  show (Abs a)     = printf "|%s|" (show a)
  show (Signum a)  = printf "sgn(%s)" (show a)
  show (Upper a b) = printf "(%s upper %s)" (show a) (show b)
  show (Lower a b) = printf "(%s lower %s)" (show a) (show b)

  show (a :** b) = printf "(%s ** %s)" (show a) (show b)
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


instance (Constable a, Num a) => Num (Math a) where
  fromInteger  = N . fromInteger
  N a + N b | is_const a && is_const b = N (a + b)
  a   + b                              = a :+ b
  N a - N b | is_const a && is_const b = N (a - b)
  a   - b                              = a :- b
  N a * N b | is_const a && is_const b = N (a * b)
  a   * b                              = a :* b
  abs (N a) | is_const a               = N (abs a)
  abs a                                = Abs a
  signum (N a) | is_const a            = N (signum a)
  signum a                             = Signum a

instance (Constable a, Fractional a) => Fractional (Math a) where
  fromRational = N . fromRational
  N a / N b | is_const a && is_const b = N (a / b)
  a   / b                              = a :/ b

instance (Constable a, Floating a) => Floating (Math a) where
  N a ** N b | is_const a && is_const b = N (a ** b)
  a   ** b                              = a :** b

  pi                       = N pi
  exp (N a)   | is_const a = N (exp a)
  exp a                    = Exp a
  log (N a)   | is_const a = N (log a)
  log a                    = Log a
  sqrt (N a)  | is_const a = N (sqrt a)
  sqrt a                   = Sqrt a

  sin (N a)   | is_const a = N (sin a)
  sin a                    = Sin a
  cos (N a)   | is_const a = N (cos a)
  cos a                    = Cos a
  tan (N a)   | is_const a = N (tan a)
  tan a                    = Tan a
  asin (N a)  | is_const a = N (asin a)
  asin a                   = Asin a
  acos (N a)  | is_const a = N (acos a)
  acos a                   = Acos a
  atan (N a)  | is_const a = N (atan a)
  atan a                   = Atan a
  sinh (N a)  | is_const a = N (sinh a)
  sinh a                   = Sinh a
  cosh (N a)  | is_const a = N (cosh a)
  cosh a                   = Cosh a
  tanh (N a)  | is_const a = N (tanh a)
  tanh a                   = Tanh a
  asinh (N a) | is_const a = N (asinh a)
  asinh a                  = Asinh a
  acosh (N a) | is_const a = N (acosh a)
  acosh a                  = Acosh a
  atanh (N a) | is_const a = N (atanh a)
  atanh a                  = Atanh a

instance (Constable a, ClosedComparable a) => ClosedComparable (Math a) where
  lower (N a) (N b) | is_const a && is_const b = N (lower a b)
  lower a     b                                = Lower a b
  upper (N a) (N b) | is_const a && is_const b = N (upper a b)
  upper a     b                                = Upper a b


-- | Evaluate a symbolic quantity using Haskell math. To do this, we need a
--   function that handles 'N' root values.
eval :: (Floating n, ClosedComparable n) => (a -> n) -> Math a -> n
eval f (N a)       = f a
eval f (a :+ b)    = eval f a + eval f b
eval f (a :- b)    = eval f a - eval f b
eval f (a :* b)    = eval f a * eval f b
eval f (a :/ b)    = eval f a / eval f b
eval f (Abs a)     = abs    $ eval f a
eval f (Signum a)  = signum $ eval f a
eval f (Upper a b) = eval f a `upper` eval f b
eval f (Lower a b) = eval f a `lower` eval f b
eval f (a :** b)   = eval f a ** eval f b
eval f (Sqrt a)    = sqrt  $ eval f a
eval f (Exp a)     = exp   $ eval f a
eval f (Log a)     = log   $ eval f a
eval f (Sin a)     = sin   $ eval f a
eval f (Cos a)     = cos   $ eval f a
eval f (Tan a)     = tan   $ eval f a
eval f (Asin a)    = asin  $ eval f a
eval f (Acos a)    = acos  $ eval f a
eval f (Atan a)    = atan  $ eval f a
eval f (Sinh a)    = sinh  $ eval f a
eval f (Cosh a)    = cosh  $ eval f a
eval f (Tanh a)    = tanh  $ eval f a
eval f (Asinh a)   = asinh $ eval f a
eval f (Acosh a)   = acosh $ eval f a
eval f (Atanh a)   = atanh $ eval f a


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
