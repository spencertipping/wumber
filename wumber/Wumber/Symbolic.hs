{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
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
--
--   TODO: A-normal form?

module Wumber.Symbolic (
  Sym(..),
  SymFn1(..),
  SymFn2(..),
  FromFloating(..),
  Constable(..),
  Roundable(..),
  Mod(..),
  Functionable(..),
  eval,
  args_in,
) where


import Data.Binary  (Binary(..))
import Data.Set     (Set(..), empty, singleton, union)
import Data.Vector  (Vector, (!))
import Foreign.Ptr  (FunPtr(..))
import GHC.Generics (Generic(..))
import Text.Printf  (printf)

import Wumber.ClosedComparable


-- | A symbolic expression whose terminal values have the specified type. 'Sym'
--   encapsulates common math operations and is written to be easy to
--   destructure.
--
--   TODO: add callbacks to Haskell and/or C functions
--   TODO: add cond/piecewise

data Sym a = N a
           | Arg !Int
           | Fn1 !SymFn1 (Sym a)
           | Fn2 !SymFn2 (Sym a) (Sym a)
  deriving (Show, Eq, Functor, Foldable, Traversable, Generic, Binary)

-- Precedences to match their arithmetic counterparts when applicable.
{-
infixl 6 :+
infixl 6 :-
infixl 7 :*
infixl 7 :/
infixl 7 :%
infixl 8 :**
-}


-- | The class of things coercible either to numbers or 'Sym' instances. This
--   makes it easier to write polymorphic code.
class FromFloating n a where from_floating :: n -> a

instance FromFloating a (Sym a) where from_floating = N . from_floating
instance FromFloating a a       where from_floating = id
instance Num a => FromFloating Integer a where from_floating = fromInteger


-- | Unary transcendental functions that would otherwise clutter up 'Sym'.
data SymFn1 = Abs
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
            | Ceil
            | Floor
            | Round
  deriving (Show, Ord, Eq, Generic, Binary, Enum)

data SymFn2 = Add
            | Subtract
            | Multiply
            | Divide
            | Mod
            | Pow
            | Upper
            | Lower
            | Atan2
  deriving (Show, Ord, Eq, Generic, Binary, Enum)


-- | Evaluate a symbolic quantity using Haskell math. To do this, we need a
--   function that handles 'Arg' values.
eval :: (Floating a, RealFloat a, Mod a, Roundable a, ClosedComparable a)
     => (Int -> a) -> Sym a -> a
eval f (N a)        = a
eval f (Arg n)      = f n
eval f (Fn1 op a)   = fn op (eval f a)
eval f (Fn2 op a b) = fn op (eval f a) (eval f b)


nan_upper x y | isNaN x = x
              | isNaN y = y
              | otherwise = upper x y

nan_lower x y | isNaN x = x
              | isNaN y = y
              | otherwise = lower x y


class Functionable x t | t -> x where
  fn :: x -> t

instance (Floating a, Roundable a, Mod a) => Functionable SymFn1 (a -> a) where
  fn Abs    = abs
  fn Signum = signum
  fn Sqrt   = sqrt
  fn Log    = log
  fn Exp    = exp
  fn Sin    = sin
  fn Cos    = cos
  fn Tan    = tan
  fn Asin   = asin
  fn Acos   = acos
  fn Atan   = atan
  fn Sinh   = sinh
  fn Cosh   = cosh
  fn Tanh   = tanh
  fn Asinh  = asinh
  fn Acosh  = acosh
  fn Atanh  = atanh
  fn Ceil   = ceil'
  fn Floor  = floor'
  fn Round  = round'

instance (Floating a, Mod a, ClosedComparable a, RealFloat a) =>
         Functionable SymFn2 (a -> a -> a) where
  fn Add      = (+)
  fn Subtract = (-)
  fn Multiply = (*)
  fn Divide   = (/)
  fn Mod      = (%)
  fn Pow      = (**)
  fn Upper    = nan_upper
  fn Lower    = nan_lower
  fn Atan2    = atan2


-- | Returns a set of all 'Arg' indexes used by an expression.
args_in :: Sym a -> Set Int
args_in (N _)       = empty
args_in (Arg x)     = singleton x
args_in (Fn1 _ a)   = args_in a
args_in (Fn2 _ a b) = args_in a `union` args_in b


-- | Values that support floating-point 'mod', but without using Haskell's
--   numeric type hierarchy to do so.
--
--   The reason we can't use Haskell types for this is that it quantifies a
--   number of operators with 'forall b. Integral b => a -> b' -- requiring us
--   to provide function implementations that would coerce our abstract symbolic
--   quantities to concrete ones if the return type dictates it.
--
--   TODO
--   Use SHA and clown-car Integer embedding to work around this

class Mod a where (%) :: a -> a -> a
infixl 7 %

instance Mod Double where (%) = c_fmod
instance Mod Float  where (%) = c_fmodf

foreign import ccall unsafe "math.h fmod"  c_fmod  :: Double -> Double -> Double
foreign import ccall unsafe "math.h fmodf" c_fmodf :: Float  -> Float  -> Float


-- | Same story as 'Mod'; Haskell's 'RealFrac' demands more concreteness than we
--   can provide.
class Roundable a where
  ceil'  :: a -> a
  floor' :: a -> a
  round' :: a -> a

instance Roundable Double where
  ceil'  = c_ceil
  floor' = c_floor
  round' = c_round

instance Roundable Float where
  ceil'  = c_ceilf
  floor' = c_floorf
  round' = c_roundf

foreign import ccall unsafe "math.h ceil"   c_ceil   :: Double -> Double
foreign import ccall unsafe "math.h ceilf"  c_ceilf  :: Float  -> Float
foreign import ccall unsafe "math.h floor"  c_floor  :: Double -> Double
foreign import ccall unsafe "math.h floorf" c_floorf :: Float  -> Float
foreign import ccall unsafe "math.h round"  c_round  :: Double -> Double
foreign import ccall unsafe "math.h roundf" c_roundf :: Float  -> Float


-- | Values that can tell you whether they are constants -- i.e. whether 'Sym'
--   should try to collapse them at construction-time.
class Constable a where is_const :: a -> Bool

instance Constable a => Constable (Sym a) where
  is_const (N a) = is_const a
  is_const _     = False

instance Constable Double where is_const _ = True
instance Constable Float  where is_const _ = True


-- Constant folding helpers
cf_unary f _  (N a) | is_const a = N (f a)
cf_unary _ op x                  = op x

cf_binary f _  (N a) (N b) | is_const a && is_const b = N (f a b)
cf_binary _ op x     y                                = op x y


instance Bounded a => Bounded (Sym a) where
  minBound = N minBound
  maxBound = N maxBound

instance (Constable a, Num a) => Num (Sym a) where
  fromInteger = N . fromInteger
  (+)    = cf_binary (+) (Fn2 Add)
  (-)    = cf_binary (-) (Fn2 Subtract)
  (*)    = cf_binary (*) (Fn2 Multiply)
  abs    = cf_unary abs    (Fn1 Abs)
  signum = cf_unary signum (Fn1 Signum)

instance (Constable a, Fractional a) => Fractional (Sym a) where
  fromRational = N . fromRational
  (/) = cf_binary (/) (Fn2 Divide)

instance (Constable a, Mod a) => Mod (Sym a) where
  (%) = cf_binary (%) (Fn2 Mod)

instance (Constable a, Floating a) => Floating (Sym a) where
  pi = N pi

  (**) = cf_binary (**) (Fn2 Pow)

  exp   = cf_unary exp   (Fn1 Exp)
  log   = cf_unary log   (Fn1 Log)
  sqrt  = cf_unary sqrt  (Fn1 Sqrt)
  sin   = cf_unary sin   (Fn1 Sin)
  cos   = cf_unary cos   (Fn1 Cos)
  tan   = cf_unary tan   (Fn1 Tan)
  asin  = cf_unary asin  (Fn1 Asin)
  acos  = cf_unary acos  (Fn1 Acos)
  atan  = cf_unary atan  (Fn1 Atan)
  sinh  = cf_unary sinh  (Fn1 Sinh)
  cosh  = cf_unary cosh  (Fn1 Cosh)
  tanh  = cf_unary tanh  (Fn1 Tanh)
  asinh = cf_unary asinh (Fn1 Asinh)
  acosh = cf_unary acosh (Fn1 Acosh)
  atanh = cf_unary atanh (Fn1 Atanh)

instance (Constable a, ClosedComparable a) => ClosedComparable (Sym a) where
  lower = cf_binary lower (Fn2 Lower)
  upper = cf_binary upper (Fn2 Upper)
