{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{-# OPTIONS_GHC -Wincomplete-patterns #-}

-- | Support for things that can be converted to functions in different forms.
module Wumber.Functionable where


import Data.Binary  (Binary)
import Data.List    (intercalate)
import GHC.Generics (Generic)

import Wumber.Fingerprint


-- | The class of objects @x@ that can be converted to functional type @t@. This
--   is intentionally general; it's used both by constant-folding logic and by
--   JIT machinery to select C function pointers.
class Functionable x t where fn :: x -> t

instance Functionable a a where fn = id

instance Functionable () a where
  fn _ = error "Functionable is disabled for the () instance"


-- | 'Show' functionality for functions. Most functions can either be shown with
--   their arguments infix (e.g. @+@), or prefix (e.g. @sin@).
class Show f => FnShow f where
  fshow_style :: f -> Maybe FnShowStyle
  fshow_fn    :: f -> String
  fshow       :: f -> [String] -> String

  fshow_fn   = show
  fshow f xs = case fshow_style f of
    Just ShowPrefix
      | [x] <- xs -> fshow_fn f ++ "(" ++ x ++ ")"
      | otherwise -> fshow_fn f ++ "(" ++ intercalate ", " xs ++ ")"
    Just ShowPostfix
      | [x] <- xs -> "(" ++ x ++ ")" ++ fshow_fn f
      | otherwise -> "(" ++ intercalate ", " xs ++ ")" ++ fshow_fn f
    Just ShowInfix -> "(" ++ intercalate (" " ++ fshow_fn f ++ " ") xs ++ ")"
    _              -> error "must implement fshow or use builtin style"

data FnShowStyle = ShowPrefix | ShowInfix | ShowPostfix
  deriving (Show, Eq, Ord, Bounded, Enum, Generic, Binary)


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
  fn Atan2 = Just atan2
  fn _     = Nothing

instance MathFnC a => Functionable MathFn (Maybe (a -> a -> a -> a)) where
  fn IfNN = Just \x a b -> if x >= 0 then a else b
  fn _    = Nothing


-- | Semi-automatic Haskell math derivation for stuff. Implementing all of the
--   instance functions by hand is a lot of work, so let's factor it all into a
--   @newtype@ instead.
newtype Math a b = Math { unMath :: a }
  deriving (Eq, Ord, Generic, Binary)

instance Show a => Show (Math a b) where show (Math x) = show x

class MathApply a where
  fn1 :: MathFn -> a -> a
  fn2 :: MathFn -> a -> a -> a


fn1' f x   = Math $ fn1 f (unMath x)
fn2' f x y = Math $ fn2 f (unMath x) (unMath y)

instance (MathApply a, MathFnC a) => Num (Math a b) where
  fromInteger = Math . fromInteger
  (+)    = fn2' Add
  (*)    = fn2' Mul
  negate = fn1' Negate
  signum = fn1' Signum
  abs    = fn1' Abs

-- TODO: the rest of these
