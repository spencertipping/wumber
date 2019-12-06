{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -funbox-strict-fields -Wincomplete-patterns #-}

-- | Wrapper types around 'Sym' that provide Haskell 'Num', 'Floating', and
--   other such support.
module Wumber.SymMath (
  SymMath(..),
  SymMathC,
  val,
  var,
  val_of
) where


import Data.Binary   (Binary)
import Data.Foldable (foldl')
import Data.Function (on)
import Data.List     (intercalate)
import Data.Vector   ((!))
import GHC.Generics  (Generic)
import Unsafe.Coerce (unsafeCoerce)

import qualified Data.Vector as V

import Wumber.AlgebraicSymFn
import Wumber.ClosedComparable
import Wumber.Fingerprint
import Wumber.Functionable
import Wumber.SymExpr
import Wumber.SymMatch


-- | Symbolic math expressions. Structurally identical to 'Sym', but typed to
--   implement Haskell numeric classes. @f@ should be specified to be
--   convertible from @MathFn@.
newtype SymMath f a = SM { unSM :: Sym (NoProfiles f) f a }

-- | The typeclass context you'll need in order to instantiate 'SymMath'.
type SymMathC f a = (Fingerprintable a,
                     Functionable MathFn f,
                     SymbolicApply (NoProfiles f) f a,
                     ValApply f a,
                     MathFnC a)


-- | Promotes a value into a 'SymMath' expression.
val = SM . sym_val

-- | Constructs a 'SymMath' expression that refers to a variable. Variables 0-3
--   are aliased as 'x_', 'y_', 'z_', and 't_' respectively.
var = SM . sym_var

x_ = var 0  -- ^ An alias for @var 0@
y_ = var 1  -- ^ An alias for @var 1@
z_ = var 2  -- ^ An alias for @var 2@
t_ = var 3  -- ^ An alias for @var 3@

-- | Reduces the symbolic expression to a constant, or 'Nothing' if it has
--   unknown dependencies.
val_of = sym_val_of . unSM


instance SymMathC MathFn a => AlgebraicSymFn (NoProfiles MathFn) MathFn a where
  left_identity Add = Just (== SymC 0)
  left_identity Mul = Just (== SymC 1)
  left_identity Pow = Just (== SymC 1)
  left_identity _   = Nothing

  right_identity Add = Just (== SymC 0)
  right_identity Mul = Just (== SymC 1)
  right_identity Pow = Just (== SymC 1)
  right_identity _   = Nothing

  commutativity Add = Just compare
  commutativity Mul = Just compare
  commutativity _   = Nothing

  associativity Add = Just $ fn_is Add
  associativity Mul = Just $ fn_is Mul
  associativity _   = Nothing


instance (Show a, FnShow f) => Show (SymMath f a) where
  show (SM (SymC x)) = show x
  show (SM (SymV i)) | i == 0 = "x_"
                     | i == 1 = "y_"
                     | i == 2 = "z_"
                     | i == 3 = "t_"
                     | otherwise = "v" ++ show i

  show (SM (SymF f xs _)) = fshow f $ map (show . SM) $ V.toList xs

instance (Fingerprintable a,
          MathFnC a,
          ProfileApply (NoProfiles MathFn) MathFn) =>
         SymbolicApply (NoProfiles MathFn) MathFn a where
  sym_apply Negate [SymF Negate xs _] = xs ! 0
  sym_apply Recip  [SymF Recip  xs _] = xs ! 0

  sym_apply f xs = sym_apply_foldwith (normalize_with sym_apply_cons) f xs

instance MathFnC a => ValApply MathFn a where
  val_apply mf [x]    | Just f <- fn mf = f x
  val_apply mf (x:xs) | Just f <- fn mf = foldl' f x xs
  val_apply mf xs = error $ "can't apply " ++ show mf
                    ++ " to list of arity " ++ show (length xs)


fn1 f a   = SM $ sym_apply (fn f) [unSM a]
fn2 f a b = SM $ sym_apply (fn f) $ map unSM [a, b]

instance SymMathC f a => Num (SymMath f a) where
  fromInteger = val . fromInteger
  (+)         = fn2 Add
  (*)         = fn2 Mul
  negate      = fn1 Negate
  abs         = fn1 Abs
  signum      = fn1 Signum

instance SymMathC f a => Fractional (SymMath f a) where
  fromRational = val . fromRational
  recip        = fn1 Recip

instance SymMathC f a => Floating (SymMath f a) where
  (**)  = fn2 Pow
  pi    = val pi
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

instance SymMathC f a => ClosedComparable (SymMath f a) where
  upper a b = SM $ sym_apply (fn IfNN) [unSM $ a - b, unSM a, unSM b]
  lower a b = SM $ sym_apply (fn IfNN) [unSM $ a - b, unSM b, unSM a]


instance Eq (SymMath f a) where
  _ == _ = error $ "SymMath instances are not comparable; use IfNN "
           ++ "(or, for structural comparison, unwrap with unSM)"

instance Ord (SymMath f a) where
  compare _ _ = error $ "SymMath instances are not comparable; use IfNN "
                ++ "(or, for structural comparison, unwrap with unSM)"


-- | A quotient/remainder function we reuse in several places.
--
--   NOTE: this results in 'divMod' and 'quotRem' that behave identically,
--   including for negative divisors. This is technically not the way 'mod' and
--   'rem' should be defined, but it lets us keep things simple for now.
--
--   TODO: is the fix as simple as having 'qr' use 'truncate' and 'divmod' use
--   'floor'?

qr a b = (q, r) where q = truncate (a / b); r = a - q*b


instance Integral Double where toInteger = truncate; quotRem = qr
instance Integral Float  where toInteger = truncate; quotRem = qr


instance SymMathC f a => Enum (SymMath f a) where
  toEnum     = fromInteger . toInteger
  fromEnum _ = error "can't collapse SymMath to Int via fromEnum"
  pred x     = x - 1
  succ x     = x + 1

instance SymMathC f a => Real (SymMath f a) where
  toRational _ = error "can't collapse SymMath to Rational via toRational"

instance SymMathC f a => Integral (SymMath f a) where
  toInteger _ = error "can't collapse SymMath to Integer; use Truncate"
  quotRem a b = (fn2 Quot a b, fn2 Rem a b)

instance SymMathC f a => RealFrac (SymMath f a) where
  properFraction a = (unsafeCoerce q, r) where (q, r) = qr a 1
  truncate         = unsafeCoerce . fn1 Truncate
  round            = unsafeCoerce . fn1 Round
  ceiling          = unsafeCoerce . fn1 Ceiling
  floor            = unsafeCoerce . fn1 Floor

instance SymMathC f a => RealFloat (SymMath f a) where
  floatRadix _     = error "floatRadix is undefined for SymMath"
  floatDigits _    = error "floatDigits is undefined for SymMath"
  floatRange _     = error "floatRange is undefined for SymMath"
  decodeFloat _    = error "decodeFloat is undefined for SymMath"
  encodeFloat _ _  = error "decodeFloat is undefined for SymMath"
  isDenormalized _ = error "isDenormalized is undefined for SymMath"
  isNegativeZero _ = error "isNegativeZero is undefined for SymMath"
  isIEEE _         = error "isIEEE is undefined for SymMath"
  isNaN _          = error "isNaN is undefined for SymMath"
  isInfinite _     = error "isInfinite is undefined for SymMath"
  atan2            = fn2 Atan2