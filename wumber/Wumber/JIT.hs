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

import Wumber.Symbolic


foreign import ccall unsafe "sys/mman.h mmap"
  mmap :: Ptr () -> CSize -> CInt -> CInt -> Fd -> COff -> IO (Ptr a)

foreign import ccall unsafe "sys/mman.h munmap"
  munmap :: Ptr () -> CSize -> IO CInt


-- | A terminal type you can use with 'Wumber.Symbolic.Sym' that JIT assemblers
--   will know what to do with.
--
--   'Const' is a constant 'Float' or 'Double' (the type is fixed within each
--   JIT context).
--
--   'Arg' refers to a numbered argument passed in as a 'Ptr r'. On AMD64 this
--   would be addressable as an offset from '%rdi'. 'r' must be 'Storable'.

data JITN r = Const !r
            | Arg   !Int


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
dbl_mathfn :: MathFn -> Maybe (FunPtr (Double -> IO Double))
dbl_mathfn Abs    = Just p_fabs
dbl_mathfn Signum = Nothing
dbl_mathfn Log    = Just p_log
dbl_mathfn Exp    = Just p_exp
dbl_mathfn Sqrt   = Just p_sqrt
dbl_mathfn Sin    = Just p_sin
dbl_mathfn Cos    = Just p_cos
dbl_mathfn Tan    = Just p_tan
dbl_mathfn Asin   = Just p_asin
dbl_mathfn Acos   = Just p_acos
dbl_mathfn Atan   = Just p_atan
dbl_mathfn Sinh   = Just p_sinh
dbl_mathfn Cosh   = Just p_cosh
dbl_mathfn Tanh   = Just p_tanh
dbl_mathfn Asinh  = Just p_asinh
dbl_mathfn Acosh  = Just p_acosh
dbl_mathfn Atanh  = Just p_atanh


-- | Converts a 'MathFn' to a 'FunPtr' to execute that operation on 'Float's.
float_mathfn :: MathFn -> Maybe (FunPtr (Float -> IO Float))
float_mathfn Abs    = Just p_fabsf
float_mathfn Signum = Nothing
float_mathfn Log    = Just p_logf
float_mathfn Exp    = Just p_expf
float_mathfn Sqrt   = Just p_sqrtf
float_mathfn Sin    = Just p_sinf
float_mathfn Cos    = Just p_cosf
float_mathfn Tan    = Just p_tanf
float_mathfn Asin   = Just p_asinf
float_mathfn Acos   = Just p_acosf
float_mathfn Atan   = Just p_atanf
float_mathfn Sinh   = Just p_sinhf
float_mathfn Cosh   = Just p_coshf
float_mathfn Tanh   = Just p_tanhf
float_mathfn Asinh  = Just p_asinhf
float_mathfn Acosh  = Just p_acoshf
float_mathfn Atanh  = Just p_atanhf


-- Math function pointers available to JIT assemblers. These are
-- platform-independent.

foreign import ccall unsafe "math.h &pow"   p_pow   :: FunPtr (Double -> Double -> IO Double)
foreign import ccall unsafe "math.h &fmax"  p_fmax  :: FunPtr (Double -> Double -> IO Double)
foreign import ccall unsafe "math.h &fmin"  p_fmin  :: FunPtr (Double -> Double -> IO Double)

foreign import ccall unsafe "math.h &powf"  p_powf  :: FunPtr (Float -> Float -> IO Float)
foreign import ccall unsafe "math.h &fmaxf" p_fmaxf :: FunPtr (Float -> Float -> IO Float)
foreign import ccall unsafe "math.h &fminf" p_fminf :: FunPtr (Float -> Float -> IO Float)

foreign import ccall unsafe "math.h &fabs"  p_fabs  :: FunPtr (Double -> IO Double)
foreign import ccall unsafe "math.h &log"   p_log   :: FunPtr (Double -> IO Double)
foreign import ccall unsafe "math.h &exp"   p_exp   :: FunPtr (Double -> IO Double)
foreign import ccall unsafe "math.h &sqrt"  p_sqrt  :: FunPtr (Double -> IO Double)
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

foreign import ccall unsafe "math.h &fabsf"  p_fabsf  :: FunPtr (Float -> IO Float)
foreign import ccall unsafe "math.h &logf"   p_logf   :: FunPtr (Float -> IO Float)
foreign import ccall unsafe "math.h &expf"   p_expf   :: FunPtr (Float -> IO Float)
foreign import ccall unsafe "math.h &sqrtf"  p_sqrtf  :: FunPtr (Float -> IO Float)
foreign import ccall unsafe "math.h &sinf"   p_sinf   :: FunPtr (Float -> IO Float)
foreign import ccall unsafe "math.h &cosf"   p_cosf   :: FunPtr (Float -> IO Float)
foreign import ccall unsafe "math.h &tanf"   p_tanf   :: FunPtr (Float -> IO Float)
foreign import ccall unsafe "math.h &asinf"  p_asinf  :: FunPtr (Float -> IO Float)
foreign import ccall unsafe "math.h &acosf"  p_acosf  :: FunPtr (Float -> IO Float)
foreign import ccall unsafe "math.h &atanf"  p_atanf  :: FunPtr (Float -> IO Float)
foreign import ccall unsafe "math.h &sinhf"  p_sinhf  :: FunPtr (Float -> IO Float)
foreign import ccall unsafe "math.h &coshf"  p_coshf  :: FunPtr (Float -> IO Float)
foreign import ccall unsafe "math.h &tanhf"  p_tanhf  :: FunPtr (Float -> IO Float)
foreign import ccall unsafe "math.h &asinhf" p_asinhf :: FunPtr (Float -> IO Float)
foreign import ccall unsafe "math.h &acoshf" p_acoshf :: FunPtr (Float -> IO Float)
foreign import ccall unsafe "math.h &atanhf" p_atanhf :: FunPtr (Float -> IO Float)
