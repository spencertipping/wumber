{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{-# OPTIONS_GHC -Wincomplete-patterns #-}

-- | Support for things that can be converted to functions in different forms.
module Wumber.Functionable where


import Data.Binary  (Binary)
import GHC.Generics (Generic)

import Wumber.Fingerprint


-- | The class of objects @x@ that can be converted to functional type @t@. This
--   is intentionally general; it's used both by constant-folding logic and by
--   JIT machinery to select C function pointers.
class Functionable x t where fn :: x -> t

instance Functionable a a where fn = id

instance Functionable () a where
  fn _ = error "Functionable is disabled for the () instance"


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
            | Pow
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

type MathFnC a = (Num a, Fractional a, Integral a, Floating a, RealFloat a)

instance Fingerprintable MathFn where fingerprint = binary_fingerprint

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
  fn Atan2 = Just atan2
  fn _     = Nothing

instance MathFnC a => Functionable MathFn (Maybe (a -> a -> a -> a)) where
  fn IfNN = Just \x a b -> if x >= 0 then a else b
  fn _    = Nothing
