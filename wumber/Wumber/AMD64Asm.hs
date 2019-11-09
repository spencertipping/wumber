{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | JIT backend for the AMD64 instruction set with SSE2. Consumes SSA from
--   JITIR and returns 'ByteString's of machine code.
module Wumber.AMD64Asm where


import Control.Monad.RWS (RWS, evalRWS, tell)
import Data.Bits
import GHC.Word          (Word8(..), Word16(..))

import qualified Data.ByteString         as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy    as BL
import qualified Foreign.Ptr             as P

import Wumber.JIT
import Wumber.JITIR


-- TODO
-- Allocate registers, keep track of register dependencies and instruction
-- latencies. You know, real JIT stuff.

type ProcessorState = ()

type Asm a = RWS () B.Builder ProcessorState a


assemble :: (SSAReg, [SSA (JITN Double)]) -> BS.ByteString
assemble (nregs, insns) = BL.toStrict (B.toLazyByteString b)
  where (_, b) = evalRWS m () ()
        m      = do setup_frame nregs
                    mapM_ assemble' insns


asm :: [Word8] -> Asm ()
asm []     = return ()
asm (x:xs) = do tell (B.word8 x); asm xs


setup_frame :: SSAReg -> Asm ()
setup_frame nregs = do
  asm [0xc8]
  tell $ B.word16LE (fromIntegral $ nregs * 8)
  asm [0x00]


assemble' :: SSA (JITN Double) -> Asm ()
assemble' (Backend r (Const x)) = do
  asm [0x48, 0xb8]
  tell $ B.doubleLE x
  asm [0x48, 0x89] -- TODO: 'mov x(%rsp)' instructions
