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
import Wumber.MathFn
import Wumber.SymExpr
import Wumber.SymMatch


-- | Symbolic math expressions. Structurally identical to 'Sym', but typed to
--   implement Haskell numeric classes. @f@ should be specified to be
--   convertible from @MathFn@.
type SymMath f a = Math (Sym (MathProfile f) f a) SymMathPhantom
data SymMathPhantom

-- TODO: support real profiles
type MathProfile f = NoProfiles f


-- | Promotes a value into a 'SymMath' expression.
val = Math . sym_val

-- | Constructs a 'SymMath' expression that refers to a variable. Variables 0-3
--   are aliased as 'x_', 'y_', 'z_', and 't_' respectively.
var = Math . sym_var

x_ = var 0          -- ^ An alias for @var 0@
y_ = var 1          -- ^ An alias for @var 1@
z_ = var 2          -- ^ An alias for @var 2@
t_ = var 3          -- ^ An alias for @var 3@

a_ = val (Math $ As 0)
b_ = val (Math $ As 1)
c_ = val (Math $ As 2)
d_ = val (Math $ As 3)

instance SymVal a b => SymVal (SymMath f a) b where val_of = val_of . unMath

instance SymVal Double Double where val_of = Just
instance SymVal Float  Float  where val_of = Just


-- | A basic 'sym_apply' implementation for math functions.
math_sym_apply :: (Num a, Fingerprintable a)
               => MathFn
               -> [Sym (MathProfile MathFn) MathFn a]
               -> Sym (MathProfile MathFn) MathFn a

math_sym_apply Negate [SymF Negate xs _] = xs ! 0
math_sym_apply Recip  [SymF Recip  xs _] = xs ! 0

math_sym_apply Add [] = sym_val 0
math_sym_apply Mul [] = sym_val 1

math_sym_apply f xs = sym_apply_cons f xs


instance (Show a, FnShow f) => Show (SymMath f a) where
  show (Math (SymC x)) = show x
  show (Math (SymV i)) | i == 0 = "x_"
                       | i == 1 = "y_"
                       | i == 2 = "z_"
                       | i == 3 = "t_"
                       | otherwise = "v" ++ show i

  show (Math (SymF f xs _)) =
    fshow f $ map (show . (Math :: Sym (MathProfile f) f a -> SymMath f a))
            $ V.toList xs

instance (Fingerprintable a,
          MathFnC a,
          SymVal a a,
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


instance (MathFnC a, SymVal a a, Fingerprintable a) =>
         MathApply a (SymMath MathFn a) where
  fn0                              = val
  fn1 f (Math x)                   = Math $ sym_apply f [x]
  fn2 f (Math x) (Math y)          = Math $ sym_apply f [x, y]
  fn3 f (Math x) (Math y) (Math z) = Math $ sym_apply f [x, y, z]
