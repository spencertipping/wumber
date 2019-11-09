{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | JIT backend for the AMD64 instruction set with SSE2. Consumes SSA from
--   JITIR and returns 'ByteString's of machine code.
module Wumber.AMD64Asm where


import Control.Monad.RWS (RWS, evalRWS, tell)
import Data.Bits
import Data.Maybe        (fromJust)
import Foreign.Ptr       (FunPtr(..), WordPtr(..), ptrToWordPtr, castFunPtrToPtr)
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

type XMMReg = Word8
type Asm a = RWS () B.Builder ProcessorState a


assemble :: (SSAReg, [SSA (JITN Double)]) -> BS.ByteString
assemble (nregs, insns) = BL.toStrict (B.toLazyByteString b)
  where (_, b) = evalRWS m () ()
        m      = do setup_frame nregs
                    mapM_ assemble' insns


-- TODO
-- 'printf'-style magic to make this DSL easier to use

asm :: [Word8] -> Asm ()
asm []     = return ()
asm (x:xs) = do tell (B.word8 x); asm xs

rbp32 :: SSAReg -> Asm ()
rbp32 s = tell $ B.word32LE (fromIntegral $ (s + 1) * (-8))


movq_mr :: SSAReg -> XMMReg -> Asm ()
movq_mr s x = do
  asm [0xf3, 0x0f, 0x7e, shiftL x 3 .|. 0x85]
  rbp32 s

movq_rm :: XMMReg -> SSAReg -> Asm ()
movq_rm x s = do
  asm [0x66, 0x0f, 0xd6, shiftL x 3 .|. 0x85]
  rbp32 s

movq_ar :: Int -> XMMReg -> Asm ()
movq_ar i x = do
  asm [0xf3, 0x0f, 0x7e, shiftL x 3 .|. 0x87]
  tell $ B.word32LE (fromIntegral $ i * 8)

call :: FunPtr a -> Asm ()
call p = do
  asm [0x48, 0xb8]
  tell $ B.word64LE (fromIntegral a)
  asm [0xff, 0xd0]

  where WordPtr a = (ptrToWordPtr $ castFunPtrToPtr p)


setup_frame :: SSAReg -> Asm ()
setup_frame nregs = do
  asm [0xc8]
  tell $ B.word16LE (fromIntegral $ nregs * 8)
  asm [0x00]


assemble' :: SSA (JITN Double) -> Asm ()
assemble' (Backend r (Const x)) = do
  asm [0x48, 0xb8]
  tell $ B.doubleLE x
  asm [0x48, 0x89, 0x85]
  rbp32 r

assemble' (Backend r (Arg i)) = do movq_ar i 0; movq_rm 0 r

assemble' (BinOp o Pow l r) = do
  movq_mr l 0
  movq_mr r 1
  call p_pow
  movq_rm 0 o

assemble' (BinOp o op l r) = do
  movq_mr l 0
  movq_mr r 1
  asm [0xf2, 0x0f, b, 0xc1]
  movq_rm 0 o

  where b = case op of Add      -> 0x58
                       Subtract -> 0x5c
                       Multiply -> 0x59
                       Divide   -> 0x5e
                       Max      -> 0x5f
                       Min      -> 0x5d
                       Pow      -> error "no SSE2 opcode for pow"

assemble' (UnOp o op r) = do
  movq_mr r 0
  call (fromJust (dbl_mathfn op))
  movq_rm 0 o

assemble' (Return o) = do
  movq_mr o 0
  asm [0xc9, 0xc3]
