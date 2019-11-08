{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | Intermediate representation for JIT. 'Sym' expressions go through this
--   layer before being emitted as machine code through a JIT backend.
module Wumber.JITIR where

import Wumber.JIT
import Wumber.Symbolic


type Unary  = MathFn
data Binary = Add
            | Subtract
            | Multiply
            | Divide
            | Pow
            | Max
            | Min
