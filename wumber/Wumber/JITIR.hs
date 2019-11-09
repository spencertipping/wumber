{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | SSA-style intermediate representation for JITable expressions. This is the
--   bulk of the compile step for 'Sym' trees; after this, backend modules like
--   'AMD64Asm' take over and emit machine code.
module Wumber.JITIR where

import Control.Monad.State (State, runState, get, modify')

import Wumber.Symbolic


type Unary  = MathFn
data Binary = Add
            | Subtract
            | Multiply
            | Divide
            | Pow
            | Max
            | Min
  deriving (Show, Eq)


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
data SSA a = Backend SSAReg a
           | BinOp   SSAReg Binary SSAReg SSAReg
           | UnOp    SSAReg Unary  SSAReg
           | Return  SSAReg
  deriving (Show, Eq)

reg_of :: SSA a -> SSAReg
reg_of (Backend r _)   = r
reg_of (BinOp r _ _ _) = r
reg_of (UnOp r _ _)    = r
reg_of (Return r)      = r


reg :: State SSAReg SSAReg
reg = do r <- get; modify' (+ 1); return r

backend :: a -> State SSAReg (SSA a)
backend a = reg >>= return . flip Backend a

bin :: Binary -> SSAReg -> SSAReg -> State SSAReg (SSA a)
bin op l r = reg >>= \o -> return $ BinOp o op l r

un :: Unary -> SSAReg -> State SSAReg (SSA a)
un op r = reg >>= \o -> return $ UnOp o op r

linearize' :: Sym a -> State SSAReg (SSAReg, [SSA a])

-- FIXME
-- If backend expressions refer to other values, we'll need to linearize those
-- within this process.
linearize' (N x) = do r <- backend x; return (reg_of r, [r])

linearize' (a :+ b)    = linbin Add a b
linearize' (a :- b)    = linbin Subtract a b
linearize' (a :* b)    = linbin Multiply a b
linearize' (a :/ b)    = linbin Divide a b
linearize' (a :** b)   = linbin Pow a b
linearize' (Upper a b) = linbin Max a b
linearize' (Lower a b) = linbin Min a b
linearize' (Math f a)  = linun f a

linbin :: Binary -> Sym a -> Sym a -> State SSAReg (SSAReg, [SSA a])
linbin op a b = do
  (ra, la) <- linearize' a
  (rb, lb) <- linearize' b
  r <- bin op ra rb
  return (reg_of r, la ++ lb ++ [r])

linun :: Unary -> Sym a -> State SSAReg (SSAReg, [SSA a])
linun op a = do
  (ra, la) <- linearize' a
  r <- un op ra
  return (reg_of r, la ++ [r])
