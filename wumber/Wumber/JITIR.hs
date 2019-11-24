{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | SSA-style intermediate representation for JITable expressions. This is the
--   bulk of the compile step for 'Sym' trees; after this, backend modules like
--   'AMD64Asm' take over and emit machine code.
--
--   This IR is designed with vectorization and instruction latencies in mind.
--   The main challenge with vectorization in particular is that we need to get
--   enough shared computation to overcome the register packing overhead. This
--   means we don't want to try to vectorize short chains of stuff, like two
--   'add' operations; we need long chains.
--
--   Most code doesn't have any long chains of shared operations outside of
--   loops, but we're quite likely to because many of our 'Sym' expressions come
--   from vector quantities.
--
--   OK, so let's talk about the object model. Any IR is likely to provide a
--   fairly linear way to produce the result we want, and any out-of-order-ness
--   is a break from this linearity.
--
--   One possibility is to form multiple parallel linear instruction streams,
--   leaving the JIT backend to round-robin and/or vectorize its way through as
--   many as it has registers to manage. This sounds simple enough, but is a
--   little more complicated in practice because not every linearized
--   subexpression uses a small number of registers (e.g.
--   'xy²z⁴ + 3y³z + √(xz + y³ + 1)'). This means each linear segment needs to
--   indicate how many scalar registers are required to evaluate it.
--   (Realistically the number is likely to be small because we don't have that
--   many levels of operator precedence -- although nested transcendentals
--   complicate things.)
--
--   Quick sidenote: any function call we make (1) isn't vectorized, and (2)
--   requires us to spill all of our active registers to the stack. This
--   obviously complicates things too, especially because not every processor
--   architecture provides the same set of intrinsics.

module Wumber.JITIR where

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

linearize :: Sym a -> (SSAReg, [SSA a])
linearize s = (n, l ++ [Return r]) where ((r, l), n) = runState (linearize' s) 0


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
reg = do r <- get; modify' (+ 1); return r

constant :: a -> State SSAReg (SSA a)
constant a = reg >>= return . flip Const a

arg :: ArgID -> State SSAReg (SSA a)
arg i = reg >>= return . flip PtrArg i

bin :: SymFn2 -> SSAReg -> SSAReg -> State SSAReg (SSA a)
bin op l r = reg >>= \o -> return $ Op2 o op l r

un :: SymFn1 -> SSAReg -> State SSAReg (SSA a)
un op r = reg >>= \o -> return $ Op1 o op r

linearize' :: Sym a -> State SSAReg (SSAReg, [SSA a])

linearize' (N x)        = do r <- constant x; return (reg_of r, [r])
linearize' (Arg i)      = do r <- arg i; return (reg_of r, [r])
linearize' (Fn1 op a)   = linun op a
linearize' (Fn2 op a b) = linbin op a b

linbin :: SymFn2 -> Sym a -> Sym a -> State SSAReg (SSAReg, [SSA a])
linbin op a b = do
  (ra, la) <- linearize' a
  (rb, lb) <- linearize' b
  r <- bin op ra rb
  return (reg_of r, la ++ lb ++ [r])

linun :: SymFn1 -> Sym a -> State SSAReg (SSAReg, [SSA a])
linun op a = do
  (ra, la) <- linearize' a
  r <- un op ra
  return (reg_of r, la ++ [r])
