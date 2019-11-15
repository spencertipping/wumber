{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE BangPatterns #-}

-- | Reverse-engineer operation timings on AMD64 processors. This allows us to
--   generalize optimization logic across multiple processor models, which is a
--   lot easier than digging through the instruction latency/throughput tables
--   and hard-coding it.
module Wumber.AMD64RE where


import Control.Monad (foldM, replicateM_, forM_)
import Control.Monad.RWS (evalRWS)
import Data.Bits
import Data.List (sort)
import Foreign.Ptr (FunPtr(..))

import qualified Data.ByteString         as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy    as BL

import Wumber.Assembler
import Wumber.AMD64Asm
import Wumber.JIT


assemble_lowlevel :: Asm a -> BS.ByteString
assemble_lowlevel m = assemble m' () ()
  where m' = do enter 0
                m
                leave_ret


-- | Serializing 'rdtsc', storing the result into all 64 bits of '%rax'. The
--   amd64 'rdtsc' instruction uses '%edx:%eax', which is very unhelpful.
rdtsc :: Asm ()
rdtsc = do
  lfence
  rdtsc_insn
  shl 2 32
  orq 3 0 2


rdtsc_start :: Asm ()
rdtsc_start = rdtsc >> hex "50"                 -- push %rax

rdtsc_end :: Asm ()
rdtsc_end = do
  rdtsc                                         -- tsc -> %rax
  hex "59"                                      -- pop %rcx
  subq 3 0 1                                    -- %rax -= %rcx


-- | Normalizes the TSC delta against a standardized amount of work. The purpose
--   of this is to correct for clock frequency and other global performance
--   factors. Result will be in %xmm0 as a dimensionless (but consistent)
--   double.
norm_tsc :: Asm ()
norm_tsc = do
  hex "50"                                      -- push %rax
  rdtsc_start
  rdtsc_end
  hex "5e"                                      -- pop %rsi
  cvtqi2sd 3 0 6
  cvtqi2sd 3 1 0
  divsd 3 0 1


rep n a = do
  rdtsc_start
  replicateM_ n a
  rdtsc_end
  norm_tsc

baseline = rep 0 (return ())


-- TODO
-- What's the model for these things? I think it's something like, we have N
-- ports and we want to know which operators use which ports and for how long.
-- What's less clear is how we differentiate between, e.g. addsd/divsd and
-- mulsd/divsd parallel conflicts (or whether we should try).
--
-- Maybe a simple way to do it is just to figure out the maximum latency per
-- operator and always schedule dependent calculations beyond that point.
-- Ideally we prefer operations that can run in parallel, though, on different
-- ports.
--
-- So: add+mul capacity? Just add? Just mul? Does div eat all ports? I think
-- these are simple "when does stuff not get faster" tests.

reptest n r m = tsc_fn_med n (rep r m)


-- If the pipeline premise is right, these two examples should have different
-- performance:
test1a = reptest 100 200 $ replicateM_ 8 $ addsd 3 0 0
test1b = reptest 100 200 $ replicateM_ 2 do
  addsd 3 0 0
  addsd 3 1 1
  addsd 3 2 2
  addsd 3 3 3

test1c = reptest 100 200 do
  addsd 3 0 0; addsd 3 4 4
  addsd 3 1 1; addsd 3 5 5
  addsd 3 2 2; addsd 3 6 6
  addsd 3 3 3; addsd 3 7 7

test1d = reptest 100 100 $ forM_ [0..15] \i -> addsd 3 i i

-- (they totally do have different performance: test1c is ~8x faster than test1a)

test2a = reptest 100 200 do
  divsd 3 0 0; divsd 3 4 4
  addsd 3 1 1; addsd 3 5 5
  addsd 3 2 2; addsd 3 6 6
  addsd 3 3 3; addsd 3 7 7

test2b = reptest 100 200 do
  divsd 3 0 0; divsd 3 0 0
  addsd 3 1 1; addsd 3 5 5
  addsd 3 2 2; addsd 3 6 6
  addsd 3 3 3; addsd 3 7 7


test2c = reptest 100 200 $ replicateM_ 8 $ divsd 3 0 0
test2d = reptest 100 200 $ replicateM_ 2 do
  divsd 3 0 0
  divsd 3 1 1
  divsd 3 2 2
  divsd 3 3 3

test2e = reptest 100 200 do
  divsd 3 0 0; divsd 3 4 4
  divsd 3 1 1; divsd 3 5 5
  divsd 3 2 2; divsd 3 6 6
  divsd 3 3 3; divsd 3 7 7


-- Latency testing: how many addsds do we need between same-register things?
-- It doesn't seem to matter. Interesting. Register dependencies matter much
-- more than instruction ordering. The main thing is that it fits into the
-- reorder buffer.
test3a = reptest 10000 200 do
  addsd 3 0 0
  addsd 3 0 0      -- zero
  forM_ [1..15] \i -> addsd 3 i i

test3b = reptest 10000 200 do
  addsd 3 0 0
  addsd 3 1 1
  addsd 3 0 0
  forM_ [2..15] \i -> addsd 3 i i

test3c = reptest 10000 200 do
  addsd 3 0 0
  addsd 3 1 1
  addsd 3 2 2
  addsd 3 3 3
  addsd 3 0 0
  forM_ [4..15] \i -> addsd 3 i i


foreign import ccall "dynamic" dfn :: FunPtr (IO Double) -> IO Double

tsc_fn n asm = do
  f <- compile dfn (assemble_lowlevel asm)
  let each !s _ = (+ s) <$> max 0 <$> f
  f
  t <- foldM each 0 [1..n]
  return $ t / fromIntegral n

tsc_fn_med n asm = do
  f <- compile dfn (assemble_lowlevel asm)
  f
  xs <- mapM (const f) [1..n]
  return $! sort xs !! (n `quot` 2)
