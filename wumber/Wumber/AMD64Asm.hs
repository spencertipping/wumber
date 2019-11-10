{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | JIT backend for the AMD64 instruction set with SSE2. Consumes SSA from
--   JITIR and returns 'ByteString's of machine code.
module Wumber.AMD64Asm where


import Control.Monad       (when)
import Control.Monad.State (StateT, execStateT)
import Control.Monad.RWS   (tell)
import Data.Bits
import Data.Maybe          (fromJust)
import Foreign.Ptr         (FunPtr(..), WordPtr(..))
import GHC.Word            (Word8(..), Word16(..))

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
type Asm    = Assembler () ProcessorState


assemble_ssa :: (SSAReg, [SSA Double]) -> BS.ByteString
assemble_ssa (nregs, insns) = assemble m () ()
  where m = do setup_frame nregs
               mapM_ assemble' insns


leave_ret = hex "c9c3"

addsd = rex0_modrm "f2" "0f58"
subsd = rex0_modrm "f2" "0f5c"
mulsd = rex0_modrm "f2" "0f59"
divsd = rex0_modrm "f2" "0f5e"
maxsd = rex0_modrm "f2" "0f5f"
minsd = rex0_modrm "f2" "0f5d"

lfence     = hex "0faee8"
rdtsc_insn = hex "0f31"

shl r bits = do rexw_modrm "" "c1" 3 4 r; tell $ B.word8 bits
orq        = rexw_modrm "" "0b"
subq       = rexw_modrm "" "2b"

cvtqi2sd = rexw_modrm "f2" "0f2a"


rex_modrm :: String -> Bool -> String -> Word8 -> Word8 -> Word8 -> Asm ()
rex_modrm ps w h mod r m = do hex ps
                              when (rexb /= 0x40) $ tell $ B.word8 rexb
                              hex h
                              tell $ B.word8 $ modrm mod r m
  where rexb = rex w r m

rex0_modrm = flip rex_modrm False
rexw_modrm = flip rex_modrm True

rex :: Bool -> Word8 -> Word8 -> Word8
rex w r b = (if w then 0x48 else 0x40)
            .|. shiftR (r .&. 0x08) 1
            .|. shiftR (b .&. 0x08) 3

modrm :: Word8 -> Word8 -> Word8 -> Word8
modrm mod r m = shiftL mod 6 .|. shiftL (r .&. 0x07) 3 .|. m .&. 0x07


rbp32 :: SSAReg -> Asm ()
rbp32 s = tell $ B.word32LE (fromIntegral $ (s + 1) * (-8))


movq_mr :: SSAReg -> XMMReg -> Asm ()
movq_mr s x = do
  rex0_modrm "f3" "0f7e" 2 x 5
  rbp32 s

movq_rm :: XMMReg -> SSAReg -> Asm ()
movq_rm x s = do
  rex0_modrm "66" "0fd6" 2 x 5
  rbp32 s

movq_ar :: Int -> XMMReg -> Asm ()
movq_ar i x = do
  rex0_modrm "f3" "0f7e" 2 x 7
  tell $ B.word32LE (fromIntegral $ i * 8)

call :: FunPtr a -> Asm ()
call p = do
  hex "57"           -- save %rdi
  hex "48b8"
  tell $ B.word64LE (fromIntegral a)
  hex "ffd0"
  hex "5f"           -- restore %rdi
  where WordPtr a = P.ptrToWordPtr $ P.castFunPtrToPtr p


setup_frame :: SSAReg -> Asm ()
setup_frame nregs = do
  hex "c8"
  tell $ B.word16LE (fromIntegral $ nregs * 8)
  hex "00"


assemble' :: SSA Double -> Asm ()
assemble' (Const r x) = do
  hex "48b8"
  tell $ B.doubleLE x
  hex "488985"
  rbp32 r

assemble' (PtrArg r i) = do
  movq_ar i 0
  movq_rm 0 r

assemble' (BinOp o op l r) = do
  movq_mr l 0
  movq_mr r 1
  case op of Pow      -> call p_pow
             Mod      -> call p_fmod
             Add      -> addsd 3 0 1
             Subtract -> subsd 3 0 1
             Multiply -> mulsd 3 0 1
             Divide   -> divsd 3 0 1
             Max      -> maxsd 3 0 1
             Min      -> minsd 3 0 1
  movq_rm 0 o

assemble' (UnOp o op r) = do
  movq_mr r 0
  call (dbl_mathfn op)
  movq_rm 0 o

assemble' (Return o) = do
  movq_mr o 0
  leave_ret
