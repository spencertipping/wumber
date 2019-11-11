{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}

module WumberTest.JIT where

import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Builder
import Data.Vector.Storable (fromList, unsafeWith)
import Foreign.Ptr (Ptr(..), FunPtr(..), WordPtr(..), ptrToWordPtr, castFunPtrToPtr)
import System.IO.Unsafe (unsafePerformIO)
import Test.QuickCheck

import Wumber.JIT


-- Test bare JIT: no assembler or anything, just the compiler and FFI.
prop_bare_jit_works :: Double -> Bool
prop_bare_jit_works x = sin (2 * x) == sin_2x x


sin_2x_code :: ByteString
sin_2x_code = toStrict $ toLazyByteString $
     mconcat (map word8 [ 0xc8, 0x00, 0x00, 0x00,       -- enter 0 0
                          0xf2, 0x0f, 0x10, 0x47, 0x08, -- movsd 8(%rdi), %xmm0
                          0xf2, 0x0f, 0x58, 0xc0,       -- addsd %xmm0, %xmm0
                          0x48, 0xb8 ])                 -- movq %rax,
  <> word64LE (fromIntegral sin_addr)                   --      imm64
  <> mconcat (map word8 [ 0xff, 0xd0,                   -- call %rax
                          0xc9,                         -- leave
                          0xc3 ])                       -- ret

  where WordPtr sin_addr = ptrToWordPtr (castFunPtrToPtr p_sin)


-- NOTE: this is the wrong way to write a function like this in reality; you'd
-- recompile it every invocation, which would defeat the purpose. For testing,
-- though, I want to exercise 'mmap' and 'munmap' multiple times.
--
-- NOTE: the [100, x] stuff is just to make sure multiple-element vectors work
-- correctly. The machine code above refers to 8(%rdi) to grab the second vector
-- element.

sin_2x :: Double -> Double
sin_2x x = unsafePerformIO do
  f <- compile dblfn sin_2x_code
  unsafeWith (fromList [100, x]) f


return []
runTests = $quickCheckAll
