{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | Algebraic simplification/rewriting for 'Sym' quantities. The most important
--   operation here is 'isolate', which attempts to isolate a variable within an
--   equation.

module Wumber.SymbolicAlgebra where


import Data.Binary  (Binary(..))
import Data.Set     (member)
import GHC.Generics (Generic(..))

import Wumber.Constraint
import Wumber.Symbolic


-- | Takes both sides of an equation and a variable to isolate, and returns a
--   mathematically sound substitution for the variable if possible. Internally,
--   all we're doing is applying function inverses until the variable ends up by
--   itself, or until we don't know how to invert an expression. Most of this
--   logic is delegated to 'Invertible'.

isolate :: (Invertible (Sym f a), SymConstraints f a)
        => VarID -> Sym f a -> Sym f a -> Maybe (Sym f a)
isolate v lhs rhs | lv && rv  = isolate v (lhs - rhs) 0
                  | rv        = isolate v rhs lhs
                  | otherwise = invert v lhs >>= \f -> return (f rhs)
  where lv = member v (vars_in lhs)
        rv = member v (vars_in rhs)


-- | Things that can be inverted, at least sometimes. Inversion happens with
--   respect to a specific variable.
class Invertible a where invert :: VarID -> a -> Maybe (a -> a)

(??) :: Maybe (a -> b) -> a -> Maybe b
Nothing ?? x = Nothing
Just f  ?? x = Just (f x)
