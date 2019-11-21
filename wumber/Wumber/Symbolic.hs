{-# LANGUAGE FlexibleInstances #-}
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
  MathFn(..),
  FromFloating(..),
  Constable(..),
  Roundable(..),
  Mod(..),
  math_fn,
  eval,
  args_in,
  binary,
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
--   TODO: move JITIR binary to a generalized unpack here

data Sym a = N a
           | Arg !Int
           | Sym a :+ Sym a
           | Sym a :- Sym a
           | Sym a :* Sym a
           | Sym a :/ Sym a
           | Sym a :% Sym a
           | Sym a :** Sym a
           | Upper (Sym a) (Sym a)
           | Lower (Sym a) (Sym a)
           | Atan2 (Sym a) (Sym a)
           | Math !MathFn (Sym a)
  deriving (Eq, Functor, Foldable, Traversable, Generic, Binary)

-- Precedences to match their arithmetic counterparts when applicable.
infixl 6 :+
infixl 6 :-
infixl 7 :*
infixl 7 :/
infixl 7 :%
infixl 8 :**


-- | The class of things coercible either to numbers or 'Sym' instances. This
--   makes it easier to write polymorphic code.
class FromFloating n a where from_floating :: n -> a

instance FromFloating a (Sym a) where from_floating = N . from_floating
instance FromFloating a a       where from_floating = id
instance Num a => FromFloating Integer a where from_floating = fromInteger


binary :: Sym a -> Maybe (Sym a, Sym a)
binary (x :+ y)    = Just (x, y)
binary (x :- y)    = Just (x, y)
binary (x :* y)    = Just (x, y)
binary (x :/ y)    = Just (x, y)
binary (x :% y)    = Just (x, y)
binary (x :** y)   = Just (x, y)
binary (Upper x y) = Just (x, y)
binary (Lower x y) = Just (x, y)
binary (Atan2 x y) = Just (x, y)
binary _           = Nothing


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
            | Ceil
            | Floor
            | Round
  deriving (Show, Ord, Eq, Generic, Binary, Enum)


-- | Evaluate a symbolic quantity using Haskell math. To do this, we need a
--   function that handles 'Arg' values.
eval :: (Floating a, RealFloat a, Mod a, Roundable a, ClosedComparable a)
     => (Int -> a) -> Sym a -> a
eval f (N a)       = a
eval f (Arg n)     = f n
eval f (a :+ b)    = eval f a + eval f b
eval f (a :- b)    = eval f a - eval f b
eval f (a :* b)    = eval f a * eval f b
eval f (a :/ b)    = eval f a / eval f b
eval f (a :% b)    = eval f a % eval f b
eval f (a :** b)   = eval f a ** eval f b
eval f (Upper a b) = eval f a `nan_upper` eval f b
eval f (Lower a b) = eval f a `nan_lower` eval f b
eval f (Atan2 a b) = eval f a `atan2` eval f b
eval f (Math m a)  = math_fn m (eval f a)


nan_upper x y | isNaN x = x
              | isNaN y = y
              | otherwise = upper x y

nan_lower x y | isNaN x = x
              | isNaN y = y
              | otherwise = lower x y


-- | Converts a 'MathFn' into a Haskell function that operates on some
--   floating-type value.
math_fn :: (Floating a, Roundable a, Mod a) => MathFn -> a -> a
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
math_fn Ceil   = ceil'
math_fn Floor  = floor'
math_fn Round  = round'


-- | Returns a set of all 'Arg' indexes used by an expression.
args_in :: Sym a -> Set Int
args_in (N _)      = empty
args_in (Arg x)    = singleton x
args_in (Math _ e) = args_in e
args_in x          = args_in x' `union` args_in y' where Just (x', y') = binary x


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


instance Show a => Show (Sym a) where
  show (N a)       = show a
  show (Arg n)     = printf "%%%d" n
  show (a :+ b)    = printf "(%s + %s)" (show a) (show b)
  show (a :- b)    = printf "(%s - %s)" (show a) (show b)
  show (a :* b)    = printf "(%s * %s)" (show a) (show b)
  show (a :/ b)    = printf "(%s / %s)" (show a) (show b)
  show (a :% b)    = printf "(%s %% %s)" (show a) (show b)
  show (a :** b)   = printf "(%s ** %s)" (show a) (show b)
  show (Upper a b) = printf "(%s upper %s)" (show a) (show b)
  show (Lower a b) = printf "(%s lower %s)" (show a) (show b)
  show (Math f a)  = printf "%s(%s)" (show f) (show a)


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
  (+)    = cf_binary (+) (:+)
  (-)    = cf_binary (-) (:-)
  (*)    = cf_binary (*) (:*)
  abs    = cf_unary abs (Math Abs)
  signum = cf_unary signum (Math Signum)

instance (Constable a, Fractional a) => Fractional (Sym a) where
  fromRational = N . fromRational
  (/) = cf_binary (/) (:/)

instance (Constable a, Mod a) => Mod (Sym a) where
  (%) = cf_binary (%) (:%)

instance (Constable a, Floating a) => Floating (Sym a) where
  pi = N pi

  (**) = cf_binary (**) (:**)

  exp   = cf_unary exp   (Math Exp)
  log   = cf_unary log   (Math Log)
  sqrt  = cf_unary sqrt  (Math Sqrt)
  sin   = cf_unary sin   (Math Sin)
  cos   = cf_unary cos   (Math Cos)
  tan   = cf_unary tan   (Math Tan)
  asin  = cf_unary asin  (Math Asin)
  acos  = cf_unary acos  (Math Acos)
  atan  = cf_unary atan  (Math Atan)
  sinh  = cf_unary sinh  (Math Sinh)
  cosh  = cf_unary cosh  (Math Cosh)
  tanh  = cf_unary tanh  (Math Tanh)
  asinh = cf_unary asinh (Math Asinh)
  acosh = cf_unary acosh (Math Acosh)
  atanh = cf_unary atanh (Math Atanh)

instance (Constable a, ClosedComparable a) => ClosedComparable (Sym a) where
  lower = cf_binary lower Lower
  upper = cf_binary upper Upper
