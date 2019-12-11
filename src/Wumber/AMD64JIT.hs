{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | JIT backend for the AMD64 instruction set with SSE2. Consumes
--   'ThreadGraph's from 'JITIR' and emits machine code.
--
--   You can calculate instruction latencies for your processor using 'AMD64RE'.
--   This should result in more accurate timing predictions, which should
--   improve instruction scheduling.

module Wumber.AMD64JIT where


import Control.Monad     (when)
import Control.Monad.RWS (asks, gets, modify')
import Data.Bits
import Data.IntMap       (IntMap(..), (!))
import Data.List         (sortOn)
import Data.Maybe        (fromJust)
import Data.Word         (Word8(..), Word16(..))
import GHC.Generics      (Generic(..), Generic1(..))

import qualified Data.ByteString as BS
import qualified Data.IntMap     as IM
import qualified Foreign.Ptr     as P

import Wumber.AMD64Asm
import Wumber.Assembler
import Wumber.JIT
import Wumber.JITIR
import Wumber.MathFn
import Wumber.Numeric


-- | AMD64 assembler monad, which tracks register assignments, deadlines, and
--   maintains a processor model that can be used to calculate expected
--   latencies. You can get a 'ProcessorModel' for your processor from
--   'AMD64RE'.
type Asm t = Assembler ProcessorModel AsmState t

type ProcessorModel = ()


-- | An encoded instruction, which is either inline machine code or a function
--   call. We care about the distinction because function calls require all xmm
--   registers to be defensively spilled to memory (even uninvolved ones). So we
--   need to model that cost when we're deciding what to schedule next.
data Encoded a = Inline (Asm a)
               | FnCall (Asm a)
  deriving (Generic, Generic1)

is_fncall            = not . is_inline
is_inline (Inline _) = True
is_inline _          = False

encoded_asm (Inline a) = a
encoded_asm (FnCall a) = a


empty_regset :: RegSet Word16
empty_regset = RS 0


-- | A model of the thread graph and processor that represents the current
--   instruction graph and a mapping from resident threads to XMM registers
--   ('as_tr').
data AsmState = AS { _as_g  :: IR MathFn Double,
                     _as_tr :: IntMap XMMReg }
  deriving (Show, Eq, Generic)


assemble_ir = error "TODO"


-- TODO
-- Rewrite everything below


{-
-- | Encodes a single instruction for the specified thread. Some context is
--   assumed; for example, inline assembly is encoded with respect to the
--   current register mapping, but function calls always assume that registers
--   have been spilled (which means arguments are always loaded from memory).
encode :: IRID -> Insn Double -> Encoded ()
encode t (LoadVal v)  = Inline $ thread_register empty_regset t >>= movconst_r v
encode t (LoadVar i)  = Inline $ thread_register empty_regset t >>= movsd_ar i
encode t (LoadThr t') = error "TODO: LoadThr"

encode t (I2 Add t')      = binop addsd t t'
encode t (I2 Subtract t') = binop subsd t t'
encode t (I2 Multiply t') = binop mulsd t t'
encode t (I2 Divide t')   = binop divsd t t'
encode t (I2 Upper t')    = binop maxsd t t'
encode t (I2 Lower t')    = binop minsd t t'

encode t (I2C Add x)      = binop_const addsd t x
encode t (I2C Subtract x) = binop_const subsd t x
encode t (I2C Multiply x) = binop_const mulsd t x
encode t (I2C Divide x)   = binop_const divsd t x
encode t (I2C Upper x)    = binop_const maxsd t x
encode t (I2C Lower x)    = binop_const minsd t x

-- TODO: optimized thread rewriting for 'Pow' cases
encode t (I2C Pow 2)      = binop mulsd t t
encode t (I2C Pow 0.5)    = encode t (I1 Sqrt)

encode t (I1 Negate) = Inline do r <- thread_register empty_regset t
                                 s <- scratch_register (regset [r])
                                 movconst_r 0 s
                                 subsd 3 s r
                                 movsd_rr s r

encode t (I1 Sqrt) = Inline do r <- thread_register empty_regset t
                               sqrtsd 3 r r

encode t (I2 f t') = FnCall do movsd_mr t  0
                               movsd_mr t' 1
                               call (fn f :: P.FunPtr (F2 Double))
                               movsd_rm 0 t

encode t (I2C f x) = FnCall do movsd_mr t 0
                               movconst_r x 1
                               call (fn f :: P.FunPtr (F2 Double))
                               movsd_rm 0 t

encode t (I1 f) = FnCall do movsd_mr t 0
                            call (fn f :: P.FunPtr (F1 Double))
                            movsd_rm 0 t


-- | Assembles a single binary operator specified by the given unspecialized
--   function (e.g. 'addsd').
binop :: (Word8 -> Word8 -> Word8 -> Asm ())
      -> IRID -> IRID -> Encoded ()
binop f t t' = Inline do s  <- thread_regset [t']
                         r  <- thread_register s t
                         tr <- gets _as_tr
                         if IM.member t' tr
                           then f 3 r (fi $ tr ! t')
                           else f 2 r rbp >> rbp32 t'


-- | Same as 'binop', but handles a constant operand instead of another
--   register.
binop_const :: (Word8 -> Word8 -> Word8 -> Asm ())
            -> IRID -> Double -> Encoded ()
binop_const f t x = Inline do r <- thread_register empty_regset t
                              s <- scratch_register (regset [r])
                              movconst_r x s
                              f 3 r s


-- | Returns a list of register-resident threads.
register_resident :: Asm [IRID]
register_resident = gets (IM.keys . _as_tr)


-- | Returns a regset for a set of threads.
thread_regset :: [IRID] -> Asm (RegSet Word16)
thread_regset ts = do tr <- gets _as_tr
                      filter (flip IM.member tr) ts
                        & map (tr !)
                        & regset
                        & return


-- | Returns a list of available registers.
free_registers :: Asm [XMMReg]
free_registers = do rs :: RegSet Word16 <- regset <$> gets (IM.elems . _as_tr)
                    return (regset_free rs)


-- | Returns a register you can use for scratch purposes, spilling a low-latency
--   thread if necessary. You can specify a series of registers that /shouldn't/
--   be spilled, usually because they're relevant operands.
scratch_register :: RegSet Word16 -> Asm XMMReg
scratch_register dontuse = do
  n  <- n_free_registers
  tr <- gets _as_tr
  if n == 0
    then IM.keys tr & filter (not . regset_member dontuse . (tr !))
                    & head
                    & spill_thread
           >> head <$> free_registers
    else head <$> free_registers


-- | Returns the number of free registers we have.
n_free_registers :: Asm Int
n_free_registers = (16 -) <$> gets (IM.size . _as_tr)


-- | Returns the register for a thread, allocating one if necessary. If no
--   registers are free, a low-latency register will be spilled to memory.
thread_register :: RegSet Word16 -> IRID -> Asm XMMReg
thread_register dontuse t = do tr <- gets _as_tr
                               if IM.member t tr
                                 then return $ tr ! t
                                 else unspill_thread dontuse t


-- | Spills a thread register to memory.
spill_thread :: IRID -> Asm ()
spill_thread t = do tr <- gets _as_tr
                    when (not (IM.member t tr))
                      $ error ("spilling a non-resident thread " ++ show (t, tr))
                    movsd_rm (tr ! t) t
                    modify' $ as_tr %~ IM.delete t


-- | Loads a thread from memory, allocating a free register for it.
unspill_thread :: RegSet Word16 -> IRID -> Asm XMMReg
unspill_thread dontuse t = do
  tr <- gets _as_tr
  when (IM.member t tr)
    $ error ("unspilling a resident thread " ++ show (t, tr))
  r <- scratch_register dontuse
  movsd_mr t r
  modify' $ as_tr %~ IM.insert t r
  return r
-}
