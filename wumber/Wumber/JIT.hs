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
  mmap :: Ptr () -> CSize -> CInt -> CInt -> Fd -> COff -> IO (Ptr a)

foreign import ccall "dynamic"
  jit_dblfn :: FunPtr (Ptr a -> IO Double) -> Ptr a -> IO Double


fi = fromIntegral

compile :: Storable a => ByteString -> IO (FunPtr (Ptr a -> IO b))
compile bs = do
  m <- mmap nullPtr (fi $ BS.length bs) 0x7 0x22 (-1) 0
  when (m == intPtrToPtr (-1)) $ return (error "mmap failed")
  unsafeUseAsCStringLen bs \(p, l) -> copyBytes m p l
  return $ unsafeCoerce m


test_fn :: Ptr Double -> IO Double
test_fn = unsafePerformIO $ jit_dblfn <$> compile (pack
  [ 0xf2, 0x0f, 0x10, 0x47, 0x00,   -- movsd 0(%rdi), %xmm0
    0xf2, 0x0f, 0x58, 0xc0,         -- addsd %xmm0, %xmm0
    0xc3 ])                         -- ret

test :: Double -> Double
test x = unsafePerformIO $ unsafeWith (singleton x) test_fn
