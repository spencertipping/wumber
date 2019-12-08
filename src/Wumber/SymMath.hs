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

-- | A @newtype@ around 'Sym' that provides Haskell 'Num', 'Floating', and other
--   such support. You can construct 'SymMath' quantities using 'val' and 'var',
--   e.g.
--
--   > x :: SymMath MathFn Double
--   > x = var 0 ** 2 + 1
--
--   Not all math operators are supported. Unsupported operators will
--   immediately throw a runtime error when used, regardless of the const-ness
--   of your symbolic expression. To extract a constant as a non-symbolic
--   quantity, use 'val_of'.

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

import Wumber.ClosedComparable
import Wumber.Fingerprint
import Wumber.Functionable
import Wumber.SymExpr
import Wumber.SymMatch


-- | Symbolic math expressions. Structurally identical to 'Sym', but typed to
--   implement Haskell numeric classes. @f@ should be specified to be
--   convertible from @MathFn@.
type SymMath f a = Math (Sym (MathProfile f) f a) SymMathPhantom
data SymMathPhantom

-- TODO: support real profiles
type MathProfile f = NoProfiles f

-- | The typeclass context you'll need in order to instantiate 'SymMath'.
type SymMathC f a = (Fingerprintable a,
                     Functionable MathFn f,
                     SymbolicApply (MathProfile f) f a,
                     ValApply f a,
                     Num a,
                     Fractional a,
                     Floating a)


-- | Promotes a value into a 'SymMath' expression.
val = Math . sym_val

-- | Constructs a 'SymMath' expression that refers to a variable. Variables 0-3
--   are aliased as 'x_', 'y_', 'z_', and 't_' respectively.
var = Math . sym_var

x_ = var 0          -- ^ An alias for @var 0@
y_ = var 1          -- ^ An alias for @var 1@
z_ = var 2          -- ^ An alias for @var 2@
t_ = var 3          -- ^ An alias for @var 3@

a_ = val (As 0)     -- ^ An alias for @val (As 0)@
b_ = val (As 1)     -- ^ An alias for @val (As 1)@
c_ = val (As 2)     -- ^ An alias for @val (As 2)@
d_ = val (As 3)     -- ^ An alias for @val (As 3)@

-- | Reduces the symbolic expression to a constant, or 'Nothing' if it has
--   unknown dependencies.
val_of = sym_val_of . unMath


-- | A basic 'sym_apply' implementation for math functions.

-- TODO
-- Normalizing equations for Add and Mul
math_sym_apply Negate [SymF Negate xs _] = xs ! 0
math_sym_apply Recip  [SymF Recip  xs _] = xs ! 0
math_sym_apply f xs = sym_apply_cons f xs


instance (Show a, FnShow f) => Show (SymMath f a) where
  show (Math (SymC x)) = show x
  show (Math (SymV i)) | i == 0 = "x_"
                       | i == 1 = "y_"
                       | i == 2 = "z_"
                       | i == 3 = "t_"
                       | otherwise = "v" ++ show i

  show (Math (SymF f xs _)) = fshow f $ map show $ V.toList xs

instance (Fingerprintable a,
          MathFnC a,
          ProfileApply (MathProfile MathFn) MathFn) =>
         SymbolicApply (MathProfile MathFn) MathFn a where
  sym_apply = sym_apply_foldwith math_sym_apply

instance MathFnC a => ValApply MathFn a where
  val_apply mf [x]    | Just f <- fn mf = f x
  val_apply mf (x:xs) | Just f <- fn mf = foldl' f x xs
  val_apply mf xs = error $ "can't apply " ++ show mf
                    ++ " to list of arity " ++ show (length xs)


instance Integral Double where toInteger = truncate; quotRem = qr
instance Integral Float  where toInteger = truncate; quotRem = qr


instance (MathFnC a, Fingerprintable a) => MathApply a (SymMath MathFn a) where
  fn0                              = val
  fn1 f (Math x)                   = Math $ sym_apply f [x]
  fn2 f (Math x) (Math y)          = Math $ sym_apply f [x, y]
  fn3 f (Math x) (Math y) (Math z) = Math $ sym_apply f [x, y, z]
