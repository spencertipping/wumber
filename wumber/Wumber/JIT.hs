{-# LANGUAGE BlockArguments #-}

module Wumber.JIT where


import Control.Monad
import Data.ByteString (ByteString, pack)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Vector.Storable (Vector, empty, singleton, unsafeWith)
import Foreign.C.Types
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Types (Fd(..), COff(..))
import Unsafe.Coerce

import qualified Data.ByteString as BS


foreign import ccall unsafe "sys/mman.h mmap"
  c_mmap :: Ptr () -> CSize -> CInt -> CInt -> Fd -> COff -> IO (Ptr a)

foreign import ccall "dynamic"
  jit_dblfn :: FunPtr (Ptr a -> IO Double) -> Ptr a -> IO Double


fi = fromIntegral

compile :: Storable a => ByteString -> IO (FunPtr (Ptr a -> IO b))
compile bs = do
  m <- c_mmap nullPtr (fi $ BS.length bs) 0x7 0x22 (-1) 0
  when (m == intPtrToPtr (-1)) $ return (error "mmap failed")
  unsafeUseAsCStringLen bs \(p, l) -> copyBytes m p l
  return $ unsafeCoerce m


invoke :: Storable a => (Ptr a -> IO b) -> Vector a -> IO b
invoke f v = unsafeWith v f


test_fn :: Ptr Double -> IO Double
test_fn = unsafePerformIO $ jit_dblfn <$> compile (pack [
  -- movsd 0(%rdi), %xmm0
  0xf2, 0x0f, 0x10, 0x47, 0x00,

  -- addsd %xmm0, %xmm0
  0xf2, 0x0f, 0x58, 0xc0,

  -- ret
  0xc3])

test :: Double -> IO Double
test x = invoke test_fn (singleton x)
