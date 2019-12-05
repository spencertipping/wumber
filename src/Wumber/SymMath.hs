{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -funbox-strict-fields -Wincomplete-patterns #-}

-- | Wrapper types around 'Sym' that provide Haskell 'Num', 'Floating', and
--   other such support.
module Wumber.SymMath where


import Data.Binary  (Binary)
import GHC.Generics (Generic)

import Wumber.Fingerprint
import Wumber.SymExpr


-- | Symbolic math expressions. Structurally identical to 'Sym', but typed to
--   implement Haskell numeric classes. @f@ should be specified to be
--   convertible from @MathFn@.
newtype SymMath f a = SM { unSM :: Sym (NoProfiles f a) f a }


-- | An enumeration of all functions Haskell provides for mathematical
--   operations. Arity isn't encoded into this list, so some functions will take
--   more arguments than others.

data MathFn = Add | Negate              -- binary functions
            | Mul | Recip
            | Rem | Quot
            | Mod | Div
            | Min | Max
            | Pow

            | Abs | Signum              -- unary functions
            | Log | Exp
            | Sin | Asin | Sinh | Asinh
            | Cos | Acos | Cosh | Acosh
            | Tan | Atan | Tanh | Atanh
            | Truncate | Round
            | Ceiling | Floor

  deriving (Show, Eq, Ord, Bounded, Enum, Generic, Binary)

instance Fingerprintable MathFn where fingerprint = binary_fingerprint


-- | The class of @f@ types you can use with 'SymMath'.
class MathFnType f where from_mathfn :: MathFn -> f

instance MathFnType MathFn where from_mathfn = id
instance (Fingerprintable a, ProfileApply p MathFn a) =>
         SymbolicApply p MathFn a where
  sym_apply = sym_apply_cons


instance (Num a, Fingerprintable a, MathFnType f,
          SymbolicApply (NoProfiles f a) f a) =>
         Num (SymMath f a) where
  fromInteger = SM . val . fromInteger
  a + b       = SM $ sym_apply (from_mathfn Add) $ map unSM [a, b]
  a * b       = SM $ sym_apply (from_mathfn Mul) $ map unSM [a, b]
  negate a    = SM $ sym_apply (from_mathfn Negate) [unSM a]

  abs a       = SM $ sym_apply (from_mathfn Abs)    [unSM a]
  signum a    = SM $ sym_apply (from_mathfn Signum) [unSM a]
