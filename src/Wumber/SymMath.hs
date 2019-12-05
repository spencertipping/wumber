{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

{-# OPTIONS_GHC -funbox-strict-fields -Wincomplete-patterns #-}

-- | Wrapper types around 'Sym' that provide Haskell 'Num', 'Floating', and
--   other such support.
module Wumber.SymMath where


import Data.Binary  (Binary)
import GHC.Generics (Generic)

import Wumber.SymExpr


-- | Symbolic math expressions. Structurally identical to 'Sym', but typed to
--   implement Haskell numeric classes. @f@ should be specified to be
--   convertible from @MathFn@.
newtype SymMath f a = SM { unSM :: Sym (NoProfiles f a) f a }


-- | An enumeration of all functions Haskell provides for mathematical
--   operations. Arity isn't encoded into this list, so some functions will take
--   more arguments than others.

data MathFn = Add | Negate              -- binary functions
            | Multiply | Recip
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


-- | The class of @f@ types you can use with 'SymMath'.
class MathFnType f where from_mathfn :: MathFn -> f

instance MathFnType MathFn where from_mathfn = id
