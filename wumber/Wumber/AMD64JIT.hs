{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | JIT backend for the AMD64 instruction set with SSE2. Consumes
--   'ThreadGraph's from 'JITIR' and emits machine code.
--
--   You can calculate instruction latencies for your processor using 'AMD64RE'.
--   This should result in more accurate timing predictions, which should
--   improve instruction scheduling.

module Wumber.AMD64JIT where


import Data.IntMap   (IntMap(..))
import Lens.Micro
import Lens.Micro.TH (makeLenses)

import qualified Data.ByteString as BS
import qualified Data.IntMap     as IM
import qualified Foreign.Ptr     as P

import Wumber.AMD64Asm
import Wumber.Assembler
import Wumber.JIT
import Wumber.JITIR
import Wumber.Symbolic


-- | AMD64 assembler monad, which tracks register assignments, deadlines, and
--   maintains a processor model that can be used to calculate expected
--   latencies. You can get a 'ProcessorModel' for your processor from
--   'AMD64RE'.
type Asm t = Assembler ProcessorModel AsmState t


-- TODO: a model that provides an expected deadline for any given instruction.
data ProcessorModel = PM { _pm_cbias :: Priority }
  deriving (Show, Eq)


-- | A model of the thread graph and processor that represents the current time
--   ('ps_t'), the estimated deadline for each XMM register ('ps_rd'), and a
--   mapping from resident threads to XMM registers ('ps_tr').
data AsmState = AS { _ps_g  :: ThreadGraph Double,
                     _ps_t  :: Double,
                     _ps_rd :: IntMap Double,
                     _ps_tr :: IntMap XMMReg }
  deriving (Show, Eq)


makeLenses ''AsmState
makeLenses ''ProcessorModel


-- | Initial 'AsmState' for the given thread graph. No threads have register
--   assignments.
init_as :: ThreadGraph Double -> AsmState
init_as g = AS g 0 all_zero IM.empty
  where all_zero = IM.fromAscList $ [ (r, 0) | r <- [0..15] ]


-- | Toplevel assembly function. Takes a 'ThreadGraph Double' and produces
--   machine code to run it. More specifically, assembles to a function whose
--   signature is 'double f(double const *)'.
assemble_graph :: ProcessorModel -> ThreadGraph Double -> BS.ByteString
assemble_graph pm tg@(TG r g) = assemble m pm (init_as tg)
  where m = do enter tg
               run_threads tg
               leave_ret r


-- | Runs threads until the return value is available.
run_threads :: ThreadGraph Double -> Asm ()
run_threads tg@(TG r g)
  | complete tg r = return ()   -- TODO: move return value to %xmm0
  | otherwise     = step >> run_threads tg
  where step = start_threads tg >> step_threads tg


-- | Starts as many threads as we have registers to support.
start_threads :: ThreadGraph Double -> Asm ()
start_threads g = return ()


-- | Steps every thread pinned to a register. If any thread completes, it is
--   unpinned from its register.
step_threads :: ThreadGraph Double -> Asm ()
step_threads g = return ()


-- | Assigns a thread to the specified register without emitting any
--   instructions (i.e. this function is just for bookkeeping). The register
--   must not be pinned to any other thread.
pin_thread :: ThreadID -> XMMReg -> Asm ()
pin_thread t r = return ()


-- | Unpins a thread from whichever register it's currently pinned to.
unpin_thread :: ThreadID -> Asm ()
unpin_thread t = return ()


{-
assemble' :: SSA Double -> Asm ()
assemble' (Const r x) = do
  hex "48b8"
  tell $ B.doubleLE x
  hex "488985"
  rbp32 r

assemble' (PtrArg r i) = do
  movsd_ar i 0
  movsd_rm 0 r

assemble' (Op2 o op l r) = do
  movsd_mr l 0
  movsd_mr r 1
  case op of Pow      -> call p_pow
             Mod      -> call p_fmod
             Atan2    -> call p_atan2
             Add      -> addsd 3 0 1
             Subtract -> subsd 3 0 1
             Multiply -> mulsd 3 0 1
             Divide   -> divsd 3 0 1
             Upper    -> maxsd 3 0 1
             Lower    -> minsd 3 0 1
  movsd_rm 0 o

assemble' (Op1 o op r) = do
  movsd_mr r 0
  call (fn op :: FunPtr (Double -> IO Double))
  movsd_rm 0 o

assemble' (Return o) = do
  movsd_mr o 0
  leave_ret
-}
