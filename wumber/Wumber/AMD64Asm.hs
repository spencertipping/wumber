{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | JIT backend for the AMD64 instruction set with SSE2. Consumes SSA from
--   JITIR and returns 'ByteString's of machine code.
module Wumber.AMD64Asm where


import Control.Monad.RWS   (tell)
import Control.Monad.State (StateT, execStateT)
import Data.Bits
import Data.Maybe        (fromJust)
import Foreign.Ptr       (FunPtr(..), WordPtr(..), ptrToWordPtr, castFunPtrToPtr)
import GHC.Word          (Word8(..), Word16(..))

import qualified Data.ByteString         as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy    as BL
import qualified Foreign.Ptr             as P

import Wumber.Assembler
import Wumber.JIT
import Wumber.JITIR


-- TODO
-- Allocate registers, keep track of register dependencies and instruction
-- latencies. You know, real JIT stuff.

type ProcessorState = ()

type XMMReg = Word8
type Asm' a = StateT ProcessorState Asm a


assemble_ssa :: (SSAReg, [SSA Double]) -> BS.ByteString
assemble_ssa (nregs, insns) = assemble $ flip execStateT () do
  setup_frame nregs
  mapM_ assemble' insns


asm :: [Word8] -> Asm' ()
asm []     = return ()
asm (x:xs) = do emit (B.word8 x); asm xs

rbp32 :: SSAReg -> Asm' ()
rbp32 s = tell $ B.word32LE (fromIntegral $ (s + 1) * (-8))


movq_mr :: SSAReg -> XMMReg -> Asm' ()
movq_mr s x = do
  asm [0xf3, 0x0f, 0x7e, shiftL x 3 .|. 0x85]
  rbp32 s

movq_rm :: XMMReg -> SSAReg -> Asm' ()
movq_rm x s = do
  asm [0x66, 0x0f, 0xd6, shiftL x 3 .|. 0x85]
  rbp32 s

movq_ar :: Int -> XMMReg -> Asm' ()
movq_ar i x = do
  asm [0xf3, 0x0f, 0x7e, shiftL x 3 .|. 0x87]
  tell $ B.word32LE (fromIntegral $ i * 8)

call :: FunPtr a -> Asm' ()
call p = do
  asm [0x48, 0xb8]
  tell $ B.word64LE (fromIntegral a)
  asm [0xff, 0xd0]

  where WordPtr a = ptrToWordPtr $ castFunPtrToPtr p


setup_frame :: SSAReg -> Asm' ()
setup_frame nregs = do
  asm [0xc8]
  tell $ B.word16LE (fromIntegral $ nregs * 8)
  asm [0x00]


assemble' :: SSA Double -> Asm' ()
assemble' (Const r x) = do
  asm [0x48, 0xb8]
  tell $ B.doubleLE x
  asm [0x48, 0x89, 0x85]
  rbp32 r

assemble' (PtrArg r i) = do
  movq_ar i 0
  movq_rm 0 r

assemble' (BinOp o op l r) = do
  movq_mr l 0
  movq_mr r 1
  case op of Pow      -> call p_pow
             Mod      -> call p_fmod
             Add      -> asm [0xf2, 0x0f, 0x58, 0xc1]
             Subtract -> asm [0xf2, 0x0f, 0x5c, 0xc1]
             Multiply -> asm [0xf2, 0x0f, 0x59, 0xc1]
             Divide   -> asm [0xf2, 0x0f, 0x5e, 0xc1]
             Max      -> asm [0xf2, 0x0f, 0x5f, 0xc1]
             Min      -> asm [0xf2, 0x0f, 0x5d, 0xc1]
  movq_rm 0 o

assemble' (UnOp o op r) = do
  movq_mr r 0
  call (dbl_mathfn op)
  movq_rm 0 o

assemble' (Return o) = do
  movq_mr o 0
  asm [0xc9, 0xc3]
