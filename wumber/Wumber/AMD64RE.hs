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


-- OK, things aren't as simple as I had hoped.
-- RDTSC unpredictably bounces between a believable number and 2x or 3x that
-- number. I suspect this has to do with CPU frequencies and/or core bouncing,
-- but I'm not sure.
--
-- I believe we can mitigate this by doing work against a consistent control
-- variable. Whatever interference is happening is low-frequency; I'll have a
-- run of 10k iterations vary by a factor of 2.

assemble_lowlevel :: Asm a -> BS.ByteString
assemble_lowlevel m = BL.toStrict (B.toLazyByteString b)
  where (_, b) = evalRWS m' () ()
        m'     = do setup_frame 0
                    m
                    asm [0xc9, 0xc3]


rdtsc_start :: Asm ()
rdtsc_start = do
  asm [0x0f, 0x31]                              -- rdtsc
  asm [0x48, 0xc1, 0xc2 .|. shiftL 4 3, 32]     -- shl %rdx, 32
  asm [0x48, 0x0b, 0xd0]                        -- or  %rax <- %rdx
  asm [0x50]                                    -- push %rax

rdtsc_end :: Asm ()
rdtsc_end = do
  asm [0x0f, 0x31]                              -- rdtsc
  asm [0x48, 0xc1, 0xc2 .|. shiftL 4 3, 32]     -- shl %rdx, 32
  asm [0x48, 0x0b, 0xd0]                        -- or  %rax <- %rdx
  asm [0x59]                                    -- pop %rcx
  asm [0x48, 0x2b, 0xc1]                        -- sub %rax <- %rcx


baseline = do rdtsc_start; rdtsc_end
addsd n = do
  rdtsc_start
  replicateM_ n $ asm [0xf2, 0x0f, 0x58, 0xc0]  -- addsd %xmm0, %xmm0
  rdtsc_end


foreign import ccall "dynamic" ifn :: FunPtr (IO Int) -> IO Int

tsc_fn asm n = with_jit ifn (assemble_lowlevel asm) \f -> do
  f
  foldM (\(!s) _ -> (min s) <$> f) maxBound [1..n]
  -- return $! fromIntegral t / fromIntegral n
