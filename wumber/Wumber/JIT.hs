{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE BangPatterns #-}

module Wumber.JIT where


import Control.Monad           (when)
import Data.ByteString         (ByteString, pack)
import Data.ByteString.Builder
import Data.ByteString.Lazy    (toStrict)
import Data.ByteString.Unsafe  (unsafeUseAsCStringLen)
import Data.Vector.Storable    (Vector, empty, fromList, unsafeWith)
import Foreign.C.Types         (CInt(..), CSize(..))
import Foreign.Marshal.Utils   (copyBytes)
import Foreign.Ptr             (Ptr(..), FunPtr(..), castPtr,
                                castPtrToFunPtr, nullPtr, intPtrToPtr)
import Foreign.Storable        (Storable)
import System.IO.Unsafe        (unsafePerformIO)
import System.Posix.Types      (Fd(..), COff(..))
import Unsafe.Coerce           (unsafeCoerce)

import qualified Data.ByteString as BS


foreign import ccall unsafe "sys/mman.h mmap"
  mmap :: Ptr () -> CSize -> CInt -> CInt -> Fd -> COff -> IO (Ptr a)

foreign import ccall unsafe "sys/mman.h munmap"
  munmap :: Ptr () -> CSize -> IO CInt

foreign import ccall "dynamic"
  dblfn :: FunPtr (Ptr a -> IO Double) -> Ptr a -> IO Double


compile :: ByteString -> IO (Ptr ())
compile bs = do
  m <- mmap nullPtr (fromIntegral $ BS.length bs) 0x7 0x22 (-1) 0
  when (m == intPtrToPtr (-1)) $ return (error "mmap failed")
  unsafeUseAsCStringLen bs \(p, l) -> copyBytes m p l
  return (castPtr m)


with_jit :: (FunPtr (a -> IO b) -> a -> IO b)
         -> ByteString -> ((a -> IO b) -> IO c) -> IO c
with_jit dynamic code f = do
  fn <- compile code
  let fn' = dynamic (castPtrToFunPtr fn)
  !x <- f fn'
  munmap fn (fromIntegral $ BS.length code)
  return x


foreign import ccall unsafe "math.h &log"   p_log   :: FunPtr (Double -> IO Double)
foreign import ccall unsafe "math.h &exp"   p_exp   :: FunPtr (Double -> IO Double)
foreign import ccall unsafe "math.h &sqrt"  p_sqrt  :: FunPtr (Double -> IO Double)
foreign import ccall unsafe "math.h &pow"   p_pow   :: FunPtr (Double -> Double -> IO Double)

foreign import ccall unsafe "math.h &sin"   p_sin   :: FunPtr (Double -> IO Double)
foreign import ccall unsafe "math.h &cos"   p_cos   :: FunPtr (Double -> IO Double)
foreign import ccall unsafe "math.h &tan"   p_tan   :: FunPtr (Double -> IO Double)
foreign import ccall unsafe "math.h &asin"  p_asin  :: FunPtr (Double -> IO Double)
foreign import ccall unsafe "math.h &acos"  p_acos  :: FunPtr (Double -> IO Double)
foreign import ccall unsafe "math.h &atan"  p_atan  :: FunPtr (Double -> IO Double)
foreign import ccall unsafe "math.h &sinh"  p_sinh  :: FunPtr (Double -> IO Double)
foreign import ccall unsafe "math.h &cosh"  p_cosh  :: FunPtr (Double -> IO Double)
foreign import ccall unsafe "math.h &tanh"  p_tanh  :: FunPtr (Double -> IO Double)
foreign import ccall unsafe "math.h &asinh" p_asinh :: FunPtr (Double -> IO Double)
foreign import ccall unsafe "math.h &acosh" p_acosh :: FunPtr (Double -> IO Double)
foreign import ccall unsafe "math.h &atanh" p_atanh :: FunPtr (Double -> IO Double)
