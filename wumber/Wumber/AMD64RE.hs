{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE BangPatterns #-}

-- | Reverse-engineer operation timings on AMD64 processors.
module Wumber.AMD64RE where


import Control.Monad (foldM, replicateM_)
import Control.Monad.RWS (evalRWS)
import Data.Bits
import Data.List (sort)
import Foreign.Ptr (FunPtr(..))

import qualified Data.ByteString         as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy    as BL

import Wumber.AMD64Asm
import Wumber.JIT


assemble_lowlevel :: Asm a -> BS.ByteString
assemble_lowlevel m = BL.toStrict (B.toLazyByteString b)
  where (_, b) = evalRWS m' () ()
        m'     = do setup_frame 0
                    m
                    asm [0xc9, 0xc3]


-- | Serializing 'rdtsc', storing the result into all 64 bits of '%rax'. The
--   'rdtsc' instruction uses '%edx:%eax', which is very unhelpful.
rdtsc :: Asm ()
rdtsc = do
  asm [0x0f, 0xae, 0xe8]                        -- lfence
  asm [0x0f, 0x31]                              -- rdtsc
  asm [0x48, 0xc1, 0xc2 .|. shiftL 4 3, 32]     -- shl %rdx, 32
  asm [0x48, 0x0b, 0xd0]                        -- or  %rax <- %rdx


rdtsc_start :: Asm ()
rdtsc_start = rdtsc >> asm [0x50]               -- push %rax

rdtsc_end :: Asm ()
rdtsc_end = do
  rdtsc                                         -- tsc -> %rax
  asm [0x59]                                    -- pop %rcx
  asm [0x48, 0x2b, 0xc1]                        -- sub %rax <- %rcx


-- | Normalizes the TSC delta against a standardized amount of work. The purpose
--   of this is to correct for clock frequency and other global performance
--   factors. Result will be in %xmm0 as a dimensionless (but consistent)
--   double.
norm_tsc :: Asm ()
norm_tsc = do
  asm [0x50]                                    -- push %rax
  rdtsc_start
  rdtsc_end
  asm [0x5e]                                    -- pop %rsi
  asm [0xf2, 0x48, 0x0f, 0x2a, 0xc6]            -- int->dbl %rsi -> %xmm0
  asm [0xf2, 0x48, 0x0f, 0x2a, 0xc8]            -- int->dbl %rax -> %xmm1
  asm [0xf2, 0x0f, 0x5e, 0xc1]                  -- %xmm0 /= %xmm1


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

addsd   = asm [0xf2, 0x0f, 0x58, 0xc0]  -- addsd %xmm0, %xmm0
addsd01 = asm [0xf2, 0x0f, 0x58, 0xc1]  -- addsd %xmm0, %xmm1
mulsd   = asm [0xf2, 0x0f, 0x59, 0xc0]  -- mulsd %xmm0, %xmm0
divsd   = asm [0xf2, 0x0f, 0x5e, 0xc0]  -- divsd %xmm0, %xmm0


foreign import ccall "dynamic" dfn :: FunPtr (IO Double) -> IO Double

tsc_fn asm n = with_jit dfn (assemble_lowlevel asm) \f -> do
  let each !s _ = (+ s) <$> max 0 <$> f
  f
  t <- foldM each 0 [1..n]
  return $ t / fromIntegral n

tsc_fn_med asm n = with_jit dfn (assemble_lowlevel asm) \f -> do
  f
  xs <- mapM (const f) [1..n]
  return $! xs !! (n `quot` 2)
