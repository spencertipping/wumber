{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -funbox-strict-fields -Wincomplete-patterns #-}

-- | Algebraic simplification/rewriting for 'SymMath' quantities. The most
--   important operation here is 'isolate', which attempts to isolate a variable
--   within an equation.
module Wumber.SymbolicAlgebra (
  isolate,
  Invertible(..)
) where


import Data.Binary  (Binary(..))
import Data.IntSet  (member)
import Data.List    (partition)
import GHC.Generics (Generic(..))

import Wumber.MathFn
import Wumber.SymExpr
import Wumber.SymMath


-- | Takes both sides of an equation and a variable to isolate, and returns a
--   mathematically sound substitution for the variable if possible. Internally,
--   all we're doing is applying function inverses until the variable ends up by
--   itself, or until we don't know how to invert an expression. Most of this
--   logic is delegated to 'Invertible'.

isolate :: SymMathC f a
        => SymMath f a -> SymMath f a -> VarID -> Maybe (SymMath f a)
isolate lhs rhs v | lv && rv  = isolate (lhs - rhs) 0 v
                  | rv        = isolate rhs lhs v
                  | otherwise = invert v lhs >>= \f -> Just (f rhs)
  where lv = member v (vars_in lhs)
        rv = member v (vars_in rhs)


-- | Things that can provide inversions against 'Sym' quantities.
class Invertible a b where invert :: VarID -> a -> Maybe (b -> b)

(^.) :: Maybe (b -> c) -> Maybe (a -> b) -> Maybe (a -> c)
Just f ^. Just g = Just (f . g)
_      ^. _      = Nothing


instance SymConstraints f a => Invertible (Sym f a) (Sym f a) where
  -- TODO
  -- Implement Cantor-Zassenhaus to try to factor polynomials when we see the
  -- variable mentioned in multiple terms.
  --
  -- https://kluedo.ub.uni-kl.de/frontdoor/deliver/index/docId/3555/file/Doktorarbeit_Martin_Lee.pdf
  invert v (ts :+ n) | length i == 1 = invert v (head i) ^. Just (+ (- (o :+ n)))
                     | otherwise     = Nothing
    where (i, o) = partition (member v . vars_in) ts

instance SymConstraints f a => Invertible (SymTerm f a) (Sym f a) where
  invert v (a :* es) | length i == 1 = invert v (head i) ^. Just (/ (sym (a :* o)))
                     | otherwise     = Nothing
    where (i, o) = partition (member v . vars_in) es

instance SymConstraints f a => Invertible (SymExp f a) (Sym f a) where
  invert v (x :** n) = invert v x ^. Just (** (val (1 / n)))

instance SymConstraints f a => Invertible (SymVar f a) (Sym f a) where
  invert v (Poly (OS x))    = invert v x
  invert v (Var i) | i == v = Just id
  invert v (Var _)          = Nothing
  invert v (Fn1 f _ (OS x)) = invert v x ^. invert v f
  invert v (FnN _ _ _)      = Nothing

  -- Most binary functions can't be inverted easily, but Pow can if either the
  -- exponent or base is constant.
  invert v (Fn2 Pow _ (OS x) (OS y))
    | not (member v (vars_in y)) = invert v x ^. Just (** (1 / y))
    | not (member v (vars_in x)) = invert v y ^. Just (\y -> log y / log x)

  invert v (Fn2 _ _ _ _) = Nothing

instance SymConstraints f a => Invertible SymFn1 (Sym f a) where
  invert _ Log    = Just exp
  invert _ Exp    = Just log
  invert _ Sin    = Just asin
  invert _ Cos    = Just acos
  invert _ Tan    = Just atan
  invert _ Asin   = Just sin
  invert _ Acos   = Just cos
  invert _ Atan   = Just tan
  invert _ Sinh   = Just asinh
  invert _ Cosh   = Just acosh
  invert _ Tanh   = Just atanh
  invert _ Asinh  = Just sinh
  invert _ Acosh  = Just cosh
  invert _ Atanh  = Just tanh
  invert _ Negate = Just negate
  invert _ Sqrt   = Just (** 2)
  invert _ _      = Nothing
