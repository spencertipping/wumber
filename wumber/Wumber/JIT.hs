{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE BangPatterns #-}

{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | Platform-independent JIT logic. This module has what you need to take a
--   'ByteString' of machine code and get a callable function from it
--   ('with_jit').
module Wumber.JIT where


import Control.Monad          (when)
import Data.Binary            (Binary(..))
import Data.ByteString        (ByteString)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Foreign.C.Types        (CInt(..), CSize(..))
import Foreign.Marshal.Utils  (copyBytes)
import Foreign.Ptr            (Ptr(..), FunPtr(..), castPtr,
                               castPtrToFunPtr, nullPtr, intPtrToPtr)
import Foreign.Storable       (Storable)
import GHC.Generics           (Generic(..))
import System.IO.Unsafe       (unsafePerformIO)
import System.Posix.Types     (Fd(..), COff(..))

import qualified Data.ByteString as BS

import Wumber.Symbolic


type F1 r = r -> IO r
type F2 r = r -> r -> IO r
type F3 r = r -> r -> r -> IO r
type F4 r = r -> r -> r -> r -> IO r

foreign import ccall "wrapper" fn1_dbl_p :: F1 Double -> IO (FunPtr (F1 Double))
foreign import ccall "wrapper" fn2_dbl_p :: F2 Double -> IO (FunPtr (F2 Double))
foreign import ccall "wrapper" fn3_dbl_p :: F3 Double -> IO (FunPtr (F3 Double))
foreign import ccall "wrapper" fn4_dbl_p :: F4 Double -> IO (FunPtr (F4 Double))

foreign import ccall "wrapper" fn1_float_p :: F1 Float -> IO (FunPtr (F1 Float))
foreign import ccall "wrapper" fn2_float_p :: F2 Float -> IO (FunPtr (F2 Float))
foreign import ccall "wrapper" fn3_float_p :: F3 Float -> IO (FunPtr (F3 Float))
foreign import ccall "wrapper" fn4_float_p :: F4 Float -> IO (FunPtr (F4 Float))


foreign import ccall "dynamic"
  dblfn :: FunPtr (Ptr a -> IO Double) -> Ptr a -> IO Double

foreign import ccall "dynamic"
  floatfn :: FunPtr (Ptr a -> IO Float) -> Ptr a -> IO Float


-- TODO
-- This module needs a lot more beef around memory management. For example, we
-- should use ForeignPtr finalizers and we should support auxiliary references,
-- e.g. to memory buffers of arguments.


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


-- | Converts a 'MathFn' to a 'FunPtr' to execute that operation on 'Double's.
dbl_mathfn :: MathFn -> FunPtr (Double -> IO Double)
dbl_mathfn Abs    = p_fabs
dbl_mathfn Signum = p_signum
dbl_mathfn Log    = p_log
dbl_mathfn Exp    = p_exp
dbl_mathfn Sqrt   = p_sqrt
dbl_mathfn Sin    = p_sin
dbl_mathfn Cos    = p_cos
dbl_mathfn Tan    = p_tan
dbl_mathfn Asin   = p_asin
dbl_mathfn Acos   = p_acos
dbl_mathfn Atan   = p_atan
dbl_mathfn Sinh   = p_sinh
dbl_mathfn Cosh   = p_cosh
dbl_mathfn Tanh   = p_tanh
dbl_mathfn Asinh  = p_asinh
dbl_mathfn Acosh  = p_acosh
dbl_mathfn Atanh  = p_atanh
dbl_mathfn Ceil   = p_ceil
dbl_mathfn Floor  = p_floor
dbl_mathfn Round  = p_round


-- | Converts a 'MathFn' to a 'FunPtr' to execute that operation on 'Float's.
float_mathfn :: MathFn -> FunPtr (Float -> IO Float)
float_mathfn Abs    = p_fabsf
float_mathfn Signum = p_signumf
float_mathfn Log    = p_logf
float_mathfn Exp    = p_expf
float_mathfn Sqrt   = p_sqrtf
float_mathfn Sin    = p_sinf
float_mathfn Cos    = p_cosf
float_mathfn Tan    = p_tanf
float_mathfn Asin   = p_asinf
float_mathfn Acos   = p_acosf
float_mathfn Atan   = p_atanf
float_mathfn Sinh   = p_sinhf
float_mathfn Cosh   = p_coshf
float_mathfn Tanh   = p_tanhf
float_mathfn Asinh  = p_asinhf
float_mathfn Acosh  = p_acoshf
float_mathfn Atanh  = p_atanhf
float_mathfn Ceil   = p_ceilf
float_mathfn Floor  = p_floorf
float_mathfn Round  = p_roundf


foreign import ccall unsafe "sys/mman.h mmap"
  mmap :: Ptr () -> CSize -> CInt -> CInt -> Fd -> COff -> IO (Ptr a)

foreign import ccall unsafe "sys/mman.h munmap"
  munmap :: Ptr () -> CSize -> IO CInt


-- Math function pointers available to JIT assemblers. These are
-- platform-independent.

p_signum  = unsafePerformIO $ fn1_dbl_p   (return . signum)
p_signumf = unsafePerformIO $ fn1_float_p (return . signum)

foreign import ccall "math.h &pow"   p_pow   :: FunPtr (Double -> Double -> IO Double)
foreign import ccall "math.h &fmod"  p_fmod  :: FunPtr (Double -> Double -> IO Double)
foreign import ccall "math.h &fmax"  p_fmax  :: FunPtr (Double -> Double -> IO Double)
foreign import ccall "math.h &fmin"  p_fmin  :: FunPtr (Double -> Double -> IO Double)

foreign import ccall "math.h &powf"  p_powf  :: FunPtr (Float -> Float -> IO Float)
foreign import ccall "math.h &fmodf" p_fmodf :: FunPtr (Float -> Float -> IO Float)
foreign import ccall "math.h &fmaxf" p_fmaxf :: FunPtr (Float -> Float -> IO Float)
foreign import ccall "math.h &fminf" p_fminf :: FunPtr (Float -> Float -> IO Float)

foreign import ccall "math.h &fabs"  p_fabs  :: FunPtr (Double -> IO Double)
foreign import ccall "math.h &log"   p_log   :: FunPtr (Double -> IO Double)
foreign import ccall "math.h &exp"   p_exp   :: FunPtr (Double -> IO Double)
foreign import ccall "math.h &sqrt"  p_sqrt  :: FunPtr (Double -> IO Double)
foreign import ccall "math.h &sin"   p_sin   :: FunPtr (Double -> IO Double)
foreign import ccall "math.h &cos"   p_cos   :: FunPtr (Double -> IO Double)
foreign import ccall "math.h &tan"   p_tan   :: FunPtr (Double -> IO Double)
foreign import ccall "math.h &asin"  p_asin  :: FunPtr (Double -> IO Double)
foreign import ccall "math.h &acos"  p_acos  :: FunPtr (Double -> IO Double)
foreign import ccall "math.h &atan"  p_atan  :: FunPtr (Double -> IO Double)
foreign import ccall "math.h &sinh"  p_sinh  :: FunPtr (Double -> IO Double)
foreign import ccall "math.h &cosh"  p_cosh  :: FunPtr (Double -> IO Double)
foreign import ccall "math.h &tanh"  p_tanh  :: FunPtr (Double -> IO Double)
foreign import ccall "math.h &asinh" p_asinh :: FunPtr (Double -> IO Double)
foreign import ccall "math.h &acosh" p_acosh :: FunPtr (Double -> IO Double)
foreign import ccall "math.h &atanh" p_atanh :: FunPtr (Double -> IO Double)
foreign import ccall "math.h &ceil"  p_ceil  :: FunPtr (Double -> IO Double)
foreign import ccall "math.h &floor" p_floor :: FunPtr (Double -> IO Double)
foreign import ccall "math.h &round" p_round :: FunPtr (Double -> IO Double)

foreign import ccall "math.h &fabsf"  p_fabsf  :: FunPtr (Float -> IO Float)
foreign import ccall "math.h &logf"   p_logf   :: FunPtr (Float -> IO Float)
foreign import ccall "math.h &expf"   p_expf   :: FunPtr (Float -> IO Float)
foreign import ccall "math.h &sqrtf"  p_sqrtf  :: FunPtr (Float -> IO Float)
foreign import ccall "math.h &sinf"   p_sinf   :: FunPtr (Float -> IO Float)
foreign import ccall "math.h &cosf"   p_cosf   :: FunPtr (Float -> IO Float)
foreign import ccall "math.h &tanf"   p_tanf   :: FunPtr (Float -> IO Float)
foreign import ccall "math.h &asinf"  p_asinf  :: FunPtr (Float -> IO Float)
foreign import ccall "math.h &acosf"  p_acosf  :: FunPtr (Float -> IO Float)
foreign import ccall "math.h &atanf"  p_atanf  :: FunPtr (Float -> IO Float)
foreign import ccall "math.h &sinhf"  p_sinhf  :: FunPtr (Float -> IO Float)
foreign import ccall "math.h &coshf"  p_coshf  :: FunPtr (Float -> IO Float)
foreign import ccall "math.h &tanhf"  p_tanhf  :: FunPtr (Float -> IO Float)
foreign import ccall "math.h &asinhf" p_asinhf :: FunPtr (Float -> IO Float)
foreign import ccall "math.h &acoshf" p_acoshf :: FunPtr (Float -> IO Float)
foreign import ccall "math.h &atanhf" p_atanhf :: FunPtr (Float -> IO Float)
foreign import ccall "math.h &ceilf"  p_ceilf  :: FunPtr (Float -> IO Float)
foreign import ccall "math.h &floorf" p_floorf :: FunPtr (Float -> IO Float)
foreign import ccall "math.h &roundf" p_roundf :: FunPtr (Float -> IO Float)
