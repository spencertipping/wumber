{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | SSA-style intermediate representation for JITable expressions. This is the
--   bulk of the compile step for 'Sym' trees; after this, backend modules like
--   'AMD64Asm' take over and emit machine code.
--
--   I haven't figured out how I want to handle auto-vectorization yet, but I do
--   want to tackle instruction latency. Basically, the idea is that we want to
--   give each register some time to settle before using the result. The
--   processor can do a lot at once if we don't have serial instruction chains.
--
--   TODO: something clever

module Wumber.JITIR (
  linearize,
  reg_of,
  SSAReg,
  SSA(..)
) where

import Control.Monad.State (State, runState, get, modify')

import Wumber.Symbolic


-- TODO
-- Figure out what kind of IR makes sense here. For example, IR shouldn't know
-- about vectorized instructions but it should provide information that's useful
-- for a vectorizing assembler: e.g. "here are two subexpressions that share an
-- operation chain".
--
-- For now, this SSA stuff is throwaway code.


-- | Linearizes a 'Sym' into a series of SSA instructions that can be assembled
--   directly. Also returns the number of SSA registers required to evaluate the
--   resulting code.
--
--   The final SSA instruction is always a 'Return', and 'Return' occurs only in
--   the final position since we don't support control flow.

linearize :: SymConstraints f a => Sym f a -> (SSAReg, [SSA a])
linearize s = (n, l ++ [Return r]) where ((r, l), n) = runState (dfs s) 0

type SSAReg = Int
data SSA a = Const  SSAReg a
           | PtrArg SSAReg Int
           | Op1    SSAReg SymFn1 SSAReg
           | Op2    SSAReg SymFn2 SSAReg SSAReg
           | Return SSAReg
  deriving (Show, Eq)

reg_of :: SSA a -> SSAReg
reg_of (Const r _)   = r
reg_of (PtrArg r _)  = r
reg_of (Op1 r _ _)   = r
reg_of (Op2 r _ _ _) = r
reg_of (Return r)    = r

reg :: State SSAReg SSAReg
reg = do r <- get
         modify' (+ 1)
         return r
