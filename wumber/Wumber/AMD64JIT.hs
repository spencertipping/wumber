{-# LANGUAGE FlexibleContexts #-}
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


import Control.Monad     (when)
import Control.Monad.RWS (asks, gets, modify')
import Data.Bits
import Data.IntMap       (IntMap(..), (!))
import Data.List         (sort)
import Data.Maybe        (fromJust)
import Lens.Micro
import Lens.Micro.TH     (makeLenses)

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


-- | An encoded instruction, which is either inline machine code or a function
--   call. We care about the distinction because function calls require all xmm
--   registers to be defensively spilled to memory (even uninvolved ones). So we
--   need to model that cost when we're deciding what to schedule next.
data Encoded a = Inline (Asm a)
               | FnCall (Asm a)

is_fncall            = not . is_inline
is_inline (Inline _) = True
is_inline _          = False

encoded_asm (Inline a) = a
encoded_asm (FnCall a) = a


-- TODO: a model that provides an expected deadline for any given instruction.
data ProcessorModel = PM { _pm_cbias :: Priority }
  deriving (Show, Eq)


-- | A model of the thread graph and processor that represents the current time
--   ('ps_t'), the estimated deadline for each XMM register ('ps_rd'), and a
--   mapping from resident threads to XMM registers ('ps_tr').
data AsmState = AS { _as_g  :: ThreadGraph Double,
                     _as_t  :: Double,
                     _as_rd :: IntMap Double,
                     _as_tr :: IntMap XMMReg }
  deriving (Show, Eq)


makeLenses ''AsmState
makeLenses ''ProcessorModel


-- | Toplevel assembly function. Takes a 'ThreadGraph Double' and produces
--   machine code to run it. More specifically, assembles to a function whose
--   signature is 'double f(double const *)'.
assemble_graph :: ProcessorModel -> ThreadGraph Double -> BS.ByteString
assemble_graph pm tg@(TG r g) = assemble m pm init_as
  where init_as  = AS tg 0 all_zero IM.empty
        all_zero = IM.fromAscList $ [ (r, 0) | r <- [0..15] ]
        m        = do enter tg
                      run_threads
                      leave_ret r


-- | Runs threads until the return value is available.
run_threads :: Asm ()
run_threads = do tg@(TG r g) <- gets _as_g
                 if complete tg r
                   then do spill_thread r
                           movsd_mr r 0
                   else do start_threads
                           step_threads
                           run_threads


-- | Starts as many threads as we have registers to support. If any
--   register-resident thread is blocked, we spill it to memory.
start_threads :: Asm ()
start_threads = spill_blocked >> start_new
  where spill_blocked = do ts <- register_resident
                           g  <- gets _as_g
                           mapM_ spill_thread $ filter (blocked g) ts
        start_new = do n <- free_registers
                       g <- gets _as_g
                       b <- asks _pm_cbias
                       mapM_ unspill_thread $ take n (map fst $ startable b g)


-- | Steps every thread pinned to a register. If any thread completes, it is
--   unpinned from its register. If any thread encodes to a function call, we
--   also unpin it /unless/ all runnable threads encode to function calls. At
--   that point we run the calls sequentially. (The logic is that this way we
--   only have to spill XMM registers once.)
step_threads :: Asm ()
step_threads = do g  <- gets _as_g
                  tr <- gets _as_tr
                  let ts = map fst $ runnable latency g
                      es = zipWith encode ts (map (peek g) ts)
                  if all is_fncall es
                    then do mapM_ spill_thread (IM.keys tr)
                            mapM_ encoded_asm es
                    else do zip ts es & filter (is_inline . snd)
                                      & mapM_ do_inline
                            mapM_ spill_if_complete (IM.keys tr)

  where peek    g t = fst (thread_step g t)
        advance g t = snd (thread_step g t)

        latency _ = 0       -- FIXME

        do_inline (t, Inline e) = do g  <- gets _as_g
                                     tr <- gets _as_tr
                                     modify' $ as_g %~ flip advance t
                                     e

        spill_if_complete t = do g <- gets _as_g
                                 when (complete g t) $ spill_thread t


-- | Encodes a single instruction for the specified thread. Some context is
--   assumed; for example, inline assembly is encoded with respect to the
--   current register mapping, but function calls always assume that registers
--   have been spilled (which means arguments are always loaded from memory).
encode :: ThreadID -> Insn Double -> Encoded ()
encode t i = Inline (return ())


-- | Returns a list of register-resident threads.
register_resident :: Asm [ThreadID]
register_resident = gets (IM.keys . _as_tr)


-- | Returns the next free register, or 'Nothing' if no registers are available.
next_free :: Asm (Maybe XMMReg)
next_free = free_from . zip [0..] <$> gets (sort . IM.elems . _as_tr)
  where free_from []                        = Nothing
        free_from ((i, r) : ps) | i /= r    = Just i
                                | otherwise = free_from ps


-- | Returns the number of free registers we have.
free_registers :: Asm Int
free_registers = (16 -) <$> gets (IM.size . _as_tr)


-- | Spills a thread to memory.
spill_thread :: ThreadID -> Asm ()
spill_thread t = do tr <- gets _as_tr
                    movsd_rm (tr ! t) t
                    unpin_thread t


-- | Loads a thread from memory, allocating a free register for it.
unspill_thread :: ThreadID -> Asm XMMReg
unspill_thread t = do r <- fromJust <$> next_free
                      movsd_mr t r
                      pin_thread t r
                      return r


-- | Assigns a thread to the specified register without emitting any
--   instructions (i.e. this function is just for bookkeeping). The register
--   must not be pinned to any other thread.
pin_thread :: ThreadID -> XMMReg -> Asm ()
pin_thread t r = modify' $ as_tr %~ IM.insert t r


-- | Unpins a thread from whichever register it's currently pinned to.
unpin_thread :: ThreadID -> Asm ()
unpin_thread t = modify' $ as_tr %~ IM.delete t


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
