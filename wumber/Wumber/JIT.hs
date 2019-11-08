{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE BangPatterns #-}

-- | Platform-independent JIT logic. This module has what you need to take a
--   'ByteString' of machine code and get a callable function from it
--   ('with_jit'). You'll need to provide the 'FunPtr (a -> IO b) -> a -> IO b'
--   dynamic.
module Wumber.JIT where


import Control.Monad          (when)
import Data.ByteString        (ByteString)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Foreign.C.Types        (CInt(..), CSize(..))
import Foreign.Marshal.Utils  (copyBytes)
import Foreign.Ptr            (Ptr(..), FunPtr(..), castPtr,
                               castPtrToFunPtr, nullPtr, intPtrToPtr)
import Foreign.Storable       (Storable)
import System.Posix.Types     (Fd(..), COff(..))

import qualified Data.ByteString as BS


foreign import ccall unsafe "sys/mman.h mmap"
  mmap :: Ptr () -> CSize -> CInt -> CInt -> Fd -> COff -> IO (Ptr a)

foreign import ccall unsafe "sys/mman.h munmap"
  munmap :: Ptr () -> CSize -> IO CInt


-- | Copies a 'ByteString' into an executable section of memory and returns a
--   pointer to the executable version. You'll need to 'munmap' this when you're
--   done, a process that's managed for you when you use 'with_jit'.
compile :: ByteString -> IO (Ptr ())
compile bs = do
  m <- mmap nullPtr (fromIntegral $ BS.length bs) 0x7 0x22 (-1) 0
  when (m == intPtrToPtr (-1)) $ return (error "mmap failed")
  unsafeUseAsCStringLen bs \(p, l) -> copyBytes m p l
  return (castPtr m)


-- | Compiles a machine code function using the specified calling convention
--   ('FunPtr a -> a') and provides it to a function. 'with_jit' unmaps the
--   compiled code after completing the IO action you return, invalidating the
--   function pointer and freeing resources.
with_jit :: (FunPtr a -> a) -> ByteString -> (a -> IO b) -> IO b
with_jit convert code f = do
  fn <- compile code
  !x <- f (convert (castPtrToFunPtr fn))
  munmap fn (fromIntegral $ BS.length code)
  return x


-- Math function pointers available to JIT assemblers. These are
-- platform-independent.

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
