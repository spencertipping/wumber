{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | Machine code assembler for AMD64 processors, mostly using the AMD64
--   instruction set. This module provides convenience but no strategy; for the
--   real beef, see 'AMD64JIT'.

module Wumber.AMD64Asm where


import Control.Monad     (when)
import Control.Monad.RWS (tell)
import Data.Bits         (shiftL, shiftR, (.|.), (.&.))
import Foreign.Ptr       (FunPtr(..), WordPtr(..))
import GHC.Word          (Word8(..), Word16(..))

import qualified Data.ByteString         as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy    as BL
import qualified Foreign.Ptr             as P

import Wumber.Assembler
import Wumber.JIT
import Wumber.JITIR
import Wumber.Numeric


-- | Strategy-oblivious assembler monad.
type Asm' t = forall a b. Assembler a b t

type XMMReg = Word8


-- | Creates a stack frame sized for the specified number of locals. Uses the
--   x86 'enter' instruction, which allows for up to 64K of local variables.
--   '%rsp' will be aligned to a 16-byte boundary.
--
--   NOTE: stack alignment is really important. If you don't do this, routines
--   like @f_sincos@ will segfault when they use AVX aligned-memory
--   instructions.

frame_enter :: Int -> Asm' ()
frame_enter n = do hex "c8"
                   tell $ B.word16LE (fromIntegral $ n * 8)
                   hex "00"
                   andqimm8 3 rsp (-16) -- align %rsp to 16-byte boundary


-- | Runs x86 'leave' and 'ret' to exit the function, returning whatever is in
--   '%xmm0'.
frame_return :: Asm' ()
frame_return = hex "c9c3"


-- | General REX-aware ModR/M encoding. This is such a pain that it's worth
--   having a function to take up most of the slack. Arguments are:
--
--   + 'ps': legacy x86 prefixes, which happen before the REX byte
--   + 'w': REX.W bit (usually informs operand width)
--   + 'h': opcode hex
--   + 'mod', 'r', 'm': ModR/M pieces

rex_modrm :: String -> Bool -> String -> Word8 -> Word8 -> Word8 -> Asm' ()
rex_modrm ps w h mod r m = do hex ps
                              when (rexb /= 0x40) $ tell $ B.word8 rexb
                              hex h
                              tell $ B.word8 $ modrm mod r m
  where rexb          = rex w r m
        modrm mod r m = shiftL mod 6 .|. shiftL (r .&. 0x07) 3 .|. m .&. 0x07
        rex w r b     = (if w then 0x48 else 0x40) .|. shiftR (r .&. 0x08) 1
                                                   .|. shiftR (b .&. 0x08) 3

rex0_modrm = flip rex_modrm False
rexw_modrm = flip rex_modrm True


-- | The '%rbp' signed displacement for the specified local variable address.
rbp32 :: IRID -> Asm' ()
rbp32 t = tell $ B.int32LE (fromIntegral $ (t + 1) * (-8))


-- | Moves the contents of one XMM register to another.
movsd_rr :: XMMReg -> XMMReg -> Asm' ()
movsd_rr r1 r2 = rex0_modrm "f3" "0f7e" 3 r2 r1

-- | Loads a local from memory into the specified XMM register.
movsd_mr :: IRID -> XMMReg -> Asm' ()
movsd_mr t x = rex0_modrm "f3" "0f7e" 2 x rbp >> rbp32 t

-- | Spills a register's value into the specified local slot.
movsd_rm :: XMMReg -> IRID -> Asm' ()
movsd_rm x t = rex0_modrm "66" "0fd6" 2 x rbp >> rbp32 t

-- | Loads a 'Var' with the specified index into the given XMM register.
movsd_ar :: Int -> XMMReg -> Asm' ()
movsd_ar i x = rex0_modrm "f3" "0f7e" 2 x rdi >> tell (B.word32LE $ fi $ i * 8)

-- | Loads the specified constant into an XMM register via '%rax'.
movconst_r :: Double -> XMMReg -> Asm' ()
movconst_r 0 r = pxor 3 r r
movconst_r x r = do hex "48b8"
                    tell $ B.doubleLE x
                    rexw_modrm "66" "0f6e" 3 r 0


-- | Calls a function specified by its absolute address. This function does
--   nothing to save or restore XMM registers; all it does is save and restore
--   '%rdi' using its preallocated stack slot.
call :: FunPtr a -> Asm' ()
call p = do hex "5657"                  -- push %rsi, %rdi
            hex "48b8"
            tell $ B.word64LE (fromIntegral a)
            hex "ffd0"
            hex "5f5e"                  -- pop %rdi, %rsi
            where WordPtr a = P.ptrToWordPtr $ P.castFunPtrToPtr p


-- SSE2

xmm = fromIntegral

addsd = rex0_modrm "f2" "0f58"
subsd = rex0_modrm "f2" "0f5c"
mulsd = rex0_modrm "f2" "0f59"
divsd = rex0_modrm "f2" "0f5e"
maxsd = rex0_modrm "f2" "0f5f"
minsd = rex0_modrm "f2" "0f5d"

addpd = rex0_modrm "66" "0f58"

pxor = rex0_modrm "66" "0fef"

sqrtsd = rex0_modrm "f2" "0f51"


-- GPR

rsp = 4
rbp = 5
rsi = 6
rdi = 7

cvtqi2sd = rexw_modrm "f2" "0f2a"

lfence     = hex "0faee8"
rdtsc_insn = hex "0f31"

movq_mr = rexw_modrm "" "8b"
movq_rm = rexw_modrm "" "89"

shl r bits       = rexw_modrm "" "c1" 3 4 r >> tell (B.word8 bits)
orq              = rexw_modrm "" "0b"
andq             = rexw_modrm "" "23"
andqimm8 mod m i = rexw_modrm "" "83" mod 4 m >> tell (B.int8 i)
subq             = rexw_modrm "" "2b"
