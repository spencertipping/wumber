{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | Algebraic simplification/rewriting for 'Sym' quantities. Internally, we
--   reduce 'Sym's to a simple normal form and try to isolate variables using
--   rewriting rules. The goal is to handle easy cases so we can bypass
--   numerical optimization or reduce dimensionality.
--
--   Almost all of this comes down to two mechanisms:
--
--   1. Normalizing forms, e.g. condensing terms in linear subsystems
--   2. Isolating and substituting variables

module Wumber.SymbolicAlgebra where


import Data.Binary  (Binary(..))
import GHC.Generics (Generic(..))

import Wumber.Constraint
import Wumber.Symbolic


-- | Normalizes a given 'Sym' by minimizing the number of tree nodes, give or
--   take. For example, 'x + x' becomes '2*x'; 'x * x' becomes 'x**2'. The goal
--   is to make it as easy as possible to isolate 'x'.
--
--   'normalize' works by converting the 'Sym' into a normal form, then decoding
--   that normal form. This is more efficient than locally rewriting.

normalize :: Num a => Sym a -> Sym a
normalize x = x


-- | The algebraic normal form for a 'Sym' tree. Really what we're doing here is
--   recognizing certain useful cases and pulling them out into specialized
--   reductions; e.g. 'Poly [a] [x] [n] b' describes the expression 'axⁿ + b'.
--   This form is useful because 'a', 'n', and 'b' are all constants, and we can
--   easily factor linear subsystems down so that each 'x' is a separate
--   variable.

-- TODO: the rest of this
data AlgNF a = Poly [a] [AlgNF a] [a] a
