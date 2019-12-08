{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wincomplete-patterns #-}

-- | 'Functionable' translation for Haskell math.
module Wumber.MathFn where


import Data.Binary   (Binary)
import Data.List     (intercalate)
import GHC.Generics  (Generic)
import Unsafe.Coerce (unsafeCoerce)

import Wumber.ClosedComparable
import Wumber.Fingerprint
import Wumber.Functionable


-- | An enumeration of all functions Haskell provides for mathematical
--   operations. Arity isn't encoded into this list, so some functions will take
--   more arguments than others.
--
--   'max' and 'min' have two problems that require us to represent them
--   differently. One is that Haskell defines them in terms of 'Ord', which
--   assumes that values can produce a concrete ordering. We can work around
--   this by using operators from 'ClosedComparable'.
--
--   The bigger problem, though, is that it isn't possible to express the
--   derivative of @max(x, y)@ using 'max'; the derivative won't necessarily
--   have the same 'max'-ness as the underlying value.
--
--   In order to keep a closed representation, we use 'IfNN' ("if nonnegative")
--   as a ternary function. The rule is that @IfNN x a b@ produces @a@ if
--   @x >= 0@, @b@ otherwise.

data MathFn = Add | Negate              -- binary functions
            | Mul | Recip
            | Rem | Quot
            | Mod | Div
            | Pow | RPow                -- NOTE: RPow == flip Pow
            | Atan2

            | Abs | Signum              -- unary functions
            | Log | Exp
            | Sin | Asin | Sinh | Asinh
            | Cos | Acos | Cosh | Acosh
            | Tan | Atan | Tanh | Atanh
            | Truncate | Round
            | Ceiling | Floor

            | IfNN                      -- ternary

  deriving (Show, Eq, Ord, Bounded, Enum, Generic, Binary)

-- | The set of constraints we need for math functions to be able to operate on
--   concrete values. This is used in 'Wumber.SymMath'.
type MathFnC a = (Num a, Fractional a, Integral a, Floating a, RealFloat a)

instance Fingerprintable MathFn where fingerprint = binary_fingerprint

instance FnShow MathFn where
  fshow_style f = Just case f of
    Add    -> ShowInfix
    Negate -> ShowPrefix
    Mul    -> ShowInfix
    Recip  -> ShowPostfix
    Rem    -> ShowInfix
    Quot   -> ShowInfix
    Mod    -> ShowInfix
    Div    -> ShowInfix
    Pow    -> ShowInfix
    RPow   -> ShowPrefix
    Atan2  -> ShowPrefix
    _      -> ShowPrefix

  fshow_fn f = case f of
    Add    -> "+"
    Negate -> "-"
    Mul    -> "*"
    Recip  -> "⁻¹"
    Rem    -> "%%"
    Quot   -> "//"
    Mod    -> "%"
    Div    -> "/"
    Pow    -> "^"
    RPow   -> "$^$"
    _      -> show f

instance MathFnC a => Functionable MathFn (Maybe (a -> a)) where
  fn Negate   = Just negate
  fn Recip    = Just recip
  fn Abs      = Just abs
  fn Signum   = Just signum
  fn Log      = Just log
  fn Exp      = Just exp
  fn Sin      = Just sin
  fn Cos      = Just cos
  fn Tan      = Just tan
  fn Asin     = Just asin
  fn Acos     = Just acos
  fn Atan     = Just atan
  fn Sinh     = Just sinh
  fn Cosh     = Just cosh
  fn Tanh     = Just tanh
  fn Asinh    = Just asinh
  fn Acosh    = Just acosh
  fn Atanh    = Just atanh
  fn Truncate = Just truncate
  fn Round    = Just round
  fn Ceiling  = Just ceiling
  fn Floor    = Just floor
  fn _        = Nothing

instance MathFnC a => Functionable MathFn (Maybe (a -> a -> a)) where
  fn Add   = Just (+)
  fn Mul   = Just (*)
  fn Rem   = Just rem
  fn Quot  = Just quot
  fn Mod   = Just mod
  fn Div   = Just div
  fn Pow   = Just (**)
  fn RPow  = Just $ flip (**)
  fn Atan2 = Just atan2
  fn _     = Nothing

instance MathFnC a => Functionable MathFn (Maybe (a -> a -> a -> a)) where
  fn IfNN = Just \x a b -> if x >= 0 then a else b
  fn _    = Nothing


-- | Semi-automatic Haskell math derivation for stuff. Implementing all of the
--   instance functions by hand is a lot of work, so let's factor it all into a
--   @newtype@ instead.
newtype Math a b = Math { unMath :: a } deriving (Generic, Binary)

instance Eq (Math a b) where
  _ == _ = error $ "Math instances are not comparable; use IfNN "
           ++ "(or, for structural comparison, unwrap with unMath)"

instance Ord (Math a b) where
  compare _ _ = error $ "Math instances are not comparable; use IfNN "
                ++ "(or, for structural comparison, unwrap with unMath)"


class MathApply a m | m -> a where
  fn0 :: a -> m
  fn1 :: MathFn -> m -> m
  fn2 :: MathFn -> m -> m -> m
  fn3 :: MathFn -> m -> m -> m -> m


instance (MathApply n (Math a b), MathFnC n) => Num (Math a b) where
  fromInteger = fn0 . fromInteger
  (+)    = fn2 Add
  (*)    = fn2 Mul
  negate = fn1 Negate
  signum = fn1 Signum
  abs    = fn1 Abs

instance (MathApply n (Math a b), MathFnC n) => Fractional (Math a b) where
  fromRational = fn0 . fromRational
  recip        = fn1 Recip

instance (MathApply n (Math a b), MathFnC n) => Floating (Math a b) where
  pi    = fn0 pi
  (**)  = fn2 Pow
  exp   = fn1 Exp
  log   = fn1 Log
  sin   = fn1 Sin
  cos   = fn1 Cos
  tan   = fn1 Tan
  asin  = fn1 Asin
  acos  = fn1 Acos
  atan  = fn1 Atan
  sinh  = fn1 Sinh
  cosh  = fn1 Cosh
  tanh  = fn1 Tanh
  asinh = fn1 Asinh
  acosh = fn1 Acosh
  atanh = fn1 Atanh

instance (MathApply n (Math a b), MathFnC n) => ClosedComparable (Math a b) where
  upper a b = fn3 IfNN (a - b) a b
  lower a b = fn3 IfNN (a - b) b a


instance (MathApply n (Math a b), MathFnC n) => Enum (Math a b) where
  toEnum     = fromInteger . toInteger
  fromEnum _ = error "can't collapse Math to Int via fromEnum"
  pred x     = x - 1
  succ x     = x + 1

instance (MathApply n (Math a b), MathFnC n) => Real (Math a b) where
  toRational _ = error "can't collapse Math to Rational via toRational"

instance (MathApply n (Math a b), MathFnC n) => Integral (Math a b) where
  toInteger _ = error "can't collapse Math to Integer; use Truncate"
  quotRem a b = (fn2 Quot a b, fn2 Rem a b)
  divMod a b  = (fn2 Div a b,  fn2 Mod a b)


-- | A quotient/remainder function we reuse in several places.
--
--   NOTE: this results in 'divMod' and 'quotRem' that behave identically,
--   including for negative divisors. This is technically not the way 'mod' and
--   'rem' should be defined, but it lets us keep things simple for now.
--
--   TODO: is the fix as simple as having 'qr' use 'truncate' and 'divmod' use
--   'floor'?

qr a b = (q, r) where q = truncate (a / b); r = a - q*b

instance (MathApply n (Math a b), MathFnC n) => RealFrac (Math a b) where
  properFraction a = (unsafeCoerce q, r) where (q, r) = qr a 1
  truncate         = unsafeCoerce . fn1 Truncate
  round            = unsafeCoerce . fn1 Round
  ceiling          = unsafeCoerce . fn1 Ceiling
  floor            = unsafeCoerce . fn1 Floor

instance (MathApply n (Math a b), MathFnC n) => RealFloat (Math a b) where
  floatRadix _     = error "floatRadix is undefined for Math"
  floatDigits _    = error "floatDigits is undefined for Math"
  floatRange _     = error "floatRange is undefined for Math"
  decodeFloat _    = error "decodeFloat is undefined for Math"
  encodeFloat _ _  = error "decodeFloat is undefined for Math"
  isDenormalized _ = error "isDenormalized is undefined for Math"
  isNegativeZero _ = error "isNegativeZero is undefined for Math"
  isIEEE _         = error "isIEEE is undefined for Math"
  isNaN _          = error "isNaN is undefined for Math"
  isInfinite _     = error "isInfinite is undefined for Math"
  atan2            = fn2 Atan2
