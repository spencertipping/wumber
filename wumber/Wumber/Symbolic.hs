{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

{-# OPTIONS_GHC -funbox-strict-fields #-}


-- | Symbolic representation of closed-form numeric expressions. 'Sym a' is an
--   expression grammar whose terminals have type 'a'. 'Sym' is used both to
--   simplify linear equations for constraints, and to JIT-compile cost and
--   isosurface functions.
--
--   'Sym' constant-folds at construction time. If you don't want this behavior,
--   have 'is_const' return 'False' for all values. (See 'Constable', which 'a'
--   must implement.)

module Wumber.Symbolic (
  Sym(..),
  MathFn(..),
  math_fn,
  eval
) where


import Data.Binary  (Binary(..))
import Data.Vector  (Vector, (!))
import Foreign.Ptr  (FunPtr(..))
import GHC.Generics (Generic(..))
import Text.Printf  (printf)

import Wumber.ClosedComparable


-- | Symbolic math operations, plus upper/lower. 'N' is a backdoor into an
--   arbitrary type that you specify. 'a' should be 'Constable'; if operands are
--   'is_const' then the operations will happen at construction time and won't
--   be present in the symbolic value.
data Sym a = N a
           | Sym a :+ Sym a
           | Sym a :- Sym a
           | Sym a :* Sym a
           | Sym a :/ Sym a
           | Sym a :** Sym a
           | Upper (Sym a) (Sym a)
           | Lower (Sym a) (Sym a)
           | Math !MathFn (Sym a)
  deriving (Eq, Functor, Foldable, Traversable, Generic, Binary)

-- Precedences to match their arithmetic counterparts.
infixl 6 :+
infixl 6 :-
infixl 7 :*
infixl 7 :/
infixl 8 :**

-- | Unary transcendental functions that would otherwise clutter up 'Sym'.
data MathFn = Abs
            | Signum
            | Sqrt
            | Log
            | Exp
            | Sin
            | Cos
            | Tan
            | Asin
            | Acos
            | Atan
            | Sinh
            | Cosh
            | Tanh
            | Asinh
            | Acosh
            | Atanh
  deriving (Show, Ord, Eq, Generic, Binary)


-- | A terminal type you can use with 'Sym' that JIT assemblers will know what
--   to do with. 'Const' is a constant 'Float' or 'Double' (the type is fixed
--   within each 'Sym' context). 'Arg' refers to a numbered argument passed in
--   as a 'Ptr r'. On AMD64 this would be addressable as an offset from '%rdi'.
--   'r' must be 'Storable'.
--
--   TODO: expand this to include calls back into arbitrary Haskell functions.
--   I'm hesitant to commit to this too soon because (1) it complicates JIT, and
--   (2) it breaks 'Binary' and therefore caching.

data ExpN r = Const !r
            | Arg   !Int
  deriving (Show, Eq, Generic, Binary)

instance Constable (ExpN r) where
  is_const (Const _) = True
  is_const (Arg _)   = False


-- | Evaluate a symbolic quantity using Haskell math. To do this, we need a
--   function that handles 'N' root values.
eval :: (Floating n, ClosedComparable n) => (a -> n) -> Sym a -> n
eval f (N a)       = f a
eval f (a :+ b)    = eval f a + eval f b
eval f (a :- b)    = eval f a - eval f b
eval f (a :* b)    = eval f a * eval f b
eval f (a :/ b)    = eval f a / eval f b
eval f (a :** b)   = eval f a ** eval f b
eval f (Upper a b) = eval f a `upper` eval f b
eval f (Lower a b) = eval f a `lower` eval f b
eval f (Math m a)  = math_fn m (eval f a)


-- | Evaluates an 'ExpN' terminal on the specified 'Vector' of input values.
eval_expn :: (Floating n, ClosedComparable n) => Vector n -> ExpN n -> n
eval_expn v (Const x) = x
eval_expn v (Arg i)   = v ! i


-- | Converts a 'MathFn' into a Haskell function that operates on some
--   floating-type value.
math_fn :: Floating a => MathFn -> a -> a
math_fn Abs    = abs
math_fn Signum = signum
math_fn Sqrt   = sqrt
math_fn Log    = log
math_fn Exp    = exp
math_fn Sin    = sin
math_fn Cos    = cos
math_fn Tan    = tan
math_fn Asin   = asin
math_fn Acos   = acos
math_fn Atan   = atan
math_fn Sinh   = sinh
math_fn Cosh   = cosh
math_fn Tanh   = tanh
math_fn Asinh  = asinh
math_fn Acosh  = acosh
math_fn Atanh  = atanh


-- | Values that can tell you whether they are constants -- i.e. whether 'Sym'
--   should try to collapse them at construction-time.
class Constable a where is_const :: a -> Bool

instance Constable a => Constable (Sym a) where
  is_const (N a) = is_const a
  is_const _     = False

instance Constable Double where is_const _ = True
instance Constable Float  where is_const _ = True


instance Show a => Show (Sym a) where
  show (N a)       = show a
  show (a :+ b)    = printf "(%s + %s)" (show a) (show b)
  show (a :- b)    = printf "(%s - %s)" (show a) (show b)
  show (a :* b)    = printf "(%s * %s)" (show a) (show b)
  show (a :/ b)    = printf "(%s / %s)" (show a) (show b)
  show (a :** b)   = printf "(%s ** %s)" (show a) (show b)
  show (Upper a b) = printf "(%s upper %s)" (show a) (show b)
  show (Lower a b) = printf "(%s lower %s)" (show a) (show b)
  show (Math f a)  = printf "%s(%s)" (show f) (show a)


instance (Constable a, Num a) => Num (Sym a) where
  fromInteger  = N . fromInteger
  N a + N b | is_const a && is_const b = N (a + b)
  a   + b                              = a :+ b
  N a - N b | is_const a && is_const b = N (a - b)
  a   - b                              = a :- b
  N a * N b | is_const a && is_const b = N (a * b)
  a   * b                              = a :* b
  abs (N a) | is_const a               = N (abs a)
  abs a                                = Math Abs a
  signum (N a) | is_const a            = N (signum a)
  signum a                             = Math Signum a

instance (Constable a, Fractional a) => Fractional (Sym a) where
  fromRational = N . fromRational
  N a / N b | is_const a && is_const b = N (a / b)
  a   / b                              = a :/ b

instance (Constable a, Floating a) => Floating (Sym a) where
  N a ** N b | is_const a && is_const b = N (a ** b)
  a   ** b                              = a :** b

  pi                       = N pi
  exp (N a)   | is_const a = N (exp a)
  exp a                    = Math Exp a
  log (N a)   | is_const a = N (log a)
  log a                    = Math Log a
  sqrt (N a)  | is_const a = N (sqrt a)
  sqrt a                   = Math Sqrt a

  sin (N a)   | is_const a = N (sin a)
  sin a                    = Math Sin a
  cos (N a)   | is_const a = N (cos a)
  cos a                    = Math Cos a
  tan (N a)   | is_const a = N (tan a)
  tan a                    = Math Tan a
  asin (N a)  | is_const a = N (asin a)
  asin a                   = Math Asin a
  acos (N a)  | is_const a = N (acos a)
  acos a                   = Math Acos a
  atan (N a)  | is_const a = N (atan a)
  atan a                   = Math Atan a
  sinh (N a)  | is_const a = N (sinh a)
  sinh a                   = Math Sinh a
  cosh (N a)  | is_const a = N (cosh a)
  cosh a                   = Math Cosh a
  tanh (N a)  | is_const a = N (tanh a)
  tanh a                   = Math Tanh a
  asinh (N a) | is_const a = N (asinh a)
  asinh a                  = Math Asinh a
  acosh (N a) | is_const a = N (acosh a)
  acosh a                  = Math Acosh a
  atanh (N a) | is_const a = N (atanh a)
  atanh a                  = Math Atanh a

instance (Constable a, ClosedComparable a) => ClosedComparable (Sym a) where
  lower (N a) (N b) | is_const a && is_const b = N (lower a b)
  lower a     b                                = Lower a b
  upper (N a) (N b) | is_const a && is_const b = N (upper a b)
  upper a     b                                = Upper a b
