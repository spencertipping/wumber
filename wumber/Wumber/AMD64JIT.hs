{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Lens.Micro
import Lens.Micro.TH     (makeLenses)

import qualified Data.ByteString as BS
import qualified Data.IntMap     as IM
import qualified Foreign.Ptr     as P

import Wumber.AMD64Asm
import Wumber.Assembler
import Wumber.JIT
import Wumber.JITIR
import Wumber.Numeric
import Wumber.Symbolic

import Debug.Trace


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


empty_regset :: RegSet Word16
empty_regset = RS 0


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
        m        = do frame_enter (n_threads tg)
                      run_threads
                      frame_return


-- | Runs threads until the return value is available; then moves the return
--   value into '%xmm0'.
run_threads :: Asm ()
run_threads = do tg@(TG ret g) <- gets _as_g
                 if complete tg ret
                   then do r <- thread_register ret
                           if r /= 0
                             then movsd_rr r 0
                             else return ()
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
        start_new = do n  <- n_free_registers
                       g  <- gets _as_g
                       tr <- gets _as_tr
                       b  <- asks _pm_cbias
                       take n (map fst $ startable b g)
                         & filter (not . flip IM.member tr)
                         & mapM_ unspill_thread


-- | Steps every thread pinned to a register. If any thread completes, it is
--   unpinned from its register. If any thread encodes to a function call, we
--   also unpin it /unless/ all runnable threads encode to function calls. At
--   that point we run the calls sequentially. (The logic is that this way we
--   only have to spill XMM registers once.)
step_threads :: Asm ()
step_threads = do g  <- gets _as_g
                  tr <- gets _as_tr
                  let ts = map fst $ take 16 $ runnable latency g
                      es = zipWith encode ts (map (peek g) ts)
                  if all is_fncall es
                    then do mapM_ spill_thread (IM.keys tr)
                            mapM_ do_fncall (zip ts es)
                    else do mapM_ do_inline (zip ts es)
                            mapM_ spill_if_complete (IM.keys tr)

  where peek    g t = fst (thread_step g t)
        advance g t = snd (thread_step g t)

        latency _ = 0       -- TODO

        -- NOTE: we could spill_if_complete inside do_inline, but we might
        -- accumulate register latency. We'll do better to encode all of the
        -- work first, then go back around and spill at the end (as we do
        -- above).
        do_inline (_, FnCall _) = return ()
        do_inline (t, Inline e) = modify' (as_g %~ flip advance t) >> e

        do_fncall (_, Inline _) = error "unexpected inline in step_threads"
        do_fncall (t, FnCall e) = modify' (as_g %~ flip advance t) >> e

        spill_if_complete t = do g  <- gets _as_g
                                 tr <- gets _as_tr
                                 when (IM.member t tr && complete g t)
                                   $ spill_thread t


-- | Encodes a single instruction for the specified thread. Some context is
--   assumed; for example, inline assembly is encoded with respect to the
--   current register mapping, but function calls always assume that registers
--   have been spilled (which means arguments are always loaded from memory).
encode :: ThreadID -> Insn Double -> Encoded ()
encode t (LoadVal v)  = Inline $ thread_register t >>= movconst_r v
encode t (LoadVar i)  = Inline $ thread_register t >>= movsd_ar i
encode t (LoadThr t') = error "TODO: LoadThr"

encode t (I2 Add t')      = binop addsd t t'
encode t (I2 Subtract t') = binop subsd t t'
encode t (I2 Multiply t') = binop mulsd t t'
encode t (I2 Divide t')   = binop divsd t t'
encode t (I2 Upper t')    = binop maxsd t t'
encode t (I2 Lower t')    = binop minsd t t'

encode t (I1 Negate) = Inline do
  r <- thread_register t
  s <- scratch_register (regset [r])
  movconst_r 0 s
  subsd 3 s r
  movsd_rr s r

encode t (I2 f t') = FnCall do
  movsd_mr t  0
  movsd_mr t' 1
  call (fn f :: P.FunPtr (Double -> Double -> IO Double))
  movsd_rm 0 t

encode t (I1 f) = FnCall do
  movsd_mr t 0
  call (fn f :: P.FunPtr (Double -> IO Double))
  movsd_rm 0 t


-- | Assembles a single binary operator specified by the given unspecialized
--   function (e.g. 'addsd').
binop :: (Word8 -> Word8 -> Word8 -> Asm ())
      -> ThreadID -> ThreadID -> Encoded ()
binop f t t' = Inline do r  <- thread_register t
                         tr <- gets _as_tr
                         if IM.member t' tr
                           then f 3 r (fi $ tr ! t')
                           else f 2 r rbp >> rbp32 t'


-- | Returns a list of register-resident threads.
register_resident :: Asm [ThreadID]
register_resident = gets (IM.keys . _as_tr)


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
    then do t' <- head <$> filter (not . regset_member dontuse . (tr !))
                       <$> threads_by_latency
            spill_thread t'
            head <$> free_registers
    else head <$> free_registers


-- | Returns the number of free registers we have.
n_free_registers :: Asm Int
n_free_registers = (16 -) <$> gets (IM.size . _as_tr)


-- | Returns the register for a thread, allocating one if necessary. If no
--   registers are free, a low-latency register will be spilled to memory.
thread_register :: ThreadID -> Asm XMMReg
thread_register t = do tr <- gets _as_tr
                       if IM.member t tr
                         then return $ tr ! t
                         else unspill_thread t


-- | Returns thread IDs sorted by increasing register latency. If you need to
--   free /n/ registers, then spilling the first /n/ elements from this list
--   should minimize delays.
threads_by_latency :: Asm [ThreadID]
threads_by_latency = do rd <- gets _as_rd
                        tr <- gets _as_tr
                        map (\t -> (t, rd ! (fi $ tr ! t))) (IM.keys tr)
                          & sortOn snd
                          & map fst
                          & return


-- | Spills a thread register to memory.
spill_thread :: ThreadID -> Asm ()
spill_thread t = do tr <- gets _as_tr
                    when (not (IM.member t tr))
                      $ error ("spilling a non-resident thread " ++ show (t, tr))
                    movsd_rm (tr ! t) t
                    modify' $ as_tr %~ IM.delete t


-- | Loads a thread from memory, allocating a free register for it.
unspill_thread :: ThreadID -> Asm XMMReg
unspill_thread t = do tr <- gets _as_tr
                      when (IM.member t tr)
                        $ error ("unspilling a resident thread " ++ show (t, tr))
                      r <- scratch_register empty_regset
                      movsd_mr t r
                      modify' $ as_tr %~ IM.insert t r
                      return r
