{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | Machine code assembler for AMD64 processors, mostly using the AMD64
--   instruction set. This module provides convenience but no strategy; for the
--   real beef, see 'AMD64JIT'.

module Wumber.AMD64Asm where


import Control.Monad       (when)
import Control.Monad.State (StateT, execStateT)
import Control.Monad.RWS   (tell)
import Data.Bits           (shiftL, shiftR, (.|.), (.&.))
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
import Wumber.Numeric
import Wumber.Symbolic


-- | Strategy-oblivious assembler monad.
type Asm' t = forall a b. Assembler a b t

type XMMReg = Word8


-- | An encoded instruction, which is either inline machine code or a function
--   call. We care about the distinction because function calls require all xmm
--   registers to be defensively spilled to memory (even uninvolved ones). So we
--   need to model that cost when we're deciding what to schedule next.
data Encoded a = Inline (Asm' a)
               | FnCall (Asm' a)


-- | Creates a stack frame sized for the specified thread graph. Uses the x86
--   'enter' instruction, which allows for up to 64K of local variables. '%rsp'
--   will be aligned to a 16-byte boundary.
enter :: ThreadGraph a -> Asm' ()
enter g = do hex "c8"
             tell $ B.word16LE (fromIntegral $ n_threads g * 8)
             hex "00"
             hex "50"                   -- at least one more slot for %rdi backup
             andqimm8 3 4 (-16)         -- align %rsp to 16-byte boundary


-- | Moves the specified thread's value into '%xmm0' to return it, and then runs
--   x86 'leave' and 'ret' to exit the function.
leave_ret :: ThreadID -> Asm' ()
leave_ret r = do -- TODO: shuffle return to xmm0
                 hex "c9c3"


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


-- | The '%rbp' signed displacement for the specified thread's local memory
--   address.
rbp32 :: ThreadID -> Asm' ()
rbp32 t = tell $ B.int32LE (fromIntegral $ (t + 1) * (-8))


-- | Loads a thread from memory into the specified XMM register.
movsd_mr :: ThreadID -> XMMReg -> Asm' ()
movsd_mr t x = rex0_modrm "f3" "0f7e" 2 x 5 >> rbp32 t

-- | Spills a register's value into the specified thread memory.
movsd_rm :: XMMReg -> ThreadID -> Asm' ()
movsd_rm x t = rex0_modrm "66" "0fd6" 2 x 5 >> rbp32 t

-- | Loads a 'Var' with the specified index into the given XMM register.
movsd_ar :: Int -> XMMReg -> Asm' ()
movsd_ar i x = rex0_modrm "f3" "0f7e" 2 x 7 >> tell (B.word32LE $ fi $ i * 8)


-- | Calls a function specified by its absolute address. This function does
--   nothing to save or restore XMM registers; all it does is save and restore
--   '%rdi' using its preallocated stack slot.
call :: FunPtr a -> Asm' ()
call p = do movq_rm 0 7 4; hex "24"         -- save %rdi
            hex "48b8"
            tell $ B.word64LE (fromIntegral a)
            hex "ffd0"
            movq_mr 0 7 4; hex "24"         -- restore %rdi
            where WordPtr a = P.ptrToWordPtr $ P.castFunPtrToPtr p


-- SSE2 instructions

addsd = rex0_modrm "f2" "0f58"
subsd = rex0_modrm "f2" "0f5c"
mulsd = rex0_modrm "f2" "0f59"
divsd = rex0_modrm "f2" "0f5e"
maxsd = rex0_modrm "f2" "0f5f"
minsd = rex0_modrm "f2" "0f5d"

addpd = rex0_modrm "66" "0f58"


-- GPR instructions

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
