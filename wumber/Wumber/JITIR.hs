{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | SSA-style intermediate representation for JITable expressions. This is the
--   bulk of the compile step for 'Sym' trees; after this, backend modules like
--   'AMD64Asm' take over and emit machine code.
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
