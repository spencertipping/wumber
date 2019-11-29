{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE BangPatterns #-}

{-# OPTIONS_GHC -funbox-strict-fields -Wincomplete-patterns #-}

-- | Platform-independent JIT logic. This module has what you need to take a
--   'ByteString' of machine code and get a callable function from it
--   ('compile_machinecode').
module Wumber.JIT where


import Control.Monad          (when)
import Data.Binary            (Binary(..))
import Data.ByteString        (ByteString)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Foreign.Concurrent     (newForeignPtr)
import Foreign.C.Types        (CInt(..), CSize(..))
import Foreign.ForeignPtr     (ForeignPtr(..), withForeignPtr)
import Foreign.Marshal.Utils  (copyBytes)
import Foreign.Ptr            (Ptr(..), FunPtr(..), castPtr,
                               castPtrToFunPtr, nullPtr, intPtrToPtr)
import Foreign.Storable       (Storable)
import GHC.Generics           (Generic(..))
import System.IO.Unsafe       (unsafePerformIO)
import System.Posix.Types     (Fd(..), COff(..))

import qualified Data.ByteString as BS

import Wumber.Symbolic


foreign import ccall unsafe "sys/mman.h mmap"
  mmap :: Ptr () -> CSize -> CInt -> CInt -> Fd -> COff -> IO (Ptr a)

foreign import ccall unsafe "sys/mman.h munmap"
  munmap :: Ptr () -> CSize -> IO CInt


foreign import ccall "dynamic"
  dblfn :: FunPtr (Ptr a -> IO Double) -> Ptr a -> IO Double

foreign import ccall "dynamic"
  floatfn :: FunPtr (Ptr a -> IO Float) -> Ptr a -> IO Float


-- | We need to move arguments across an IO boundary; otherwise 'compile' will
--   be fixed to a specific JIT arity.
class ForeignPtrClosure a where functify :: ForeignPtr f -> (Ptr f -> a) -> a

instance ForeignPtrClosure (IO a) where
  functify = withForeignPtr

instance ForeignPtrClosure b => ForeignPtrClosure (a -> b) where
  functify p f x = let f' x' = f x' x in functify p f'


-- | Copies a 'ByteString' into an executable section of memory and returns a
--   function that uses Haskell's calling convention. Memory is managed by a
--   'ForeignPtr' closed over by the resulting function.
compile_machinecode :: ForeignPtrClosure a
                    => (FunPtr a -> a) -> ByteString -> IO a
compile_machinecode funptr_converter bs = do
  m    <- codeptr bs
  fptr <- newForeignPtr m (finalize (BS.length bs) m)
  return $ functify fptr (funptr_converter . castPtrToFunPtr)

  where finalize l p = munmap p (fromIntegral l) >> return ()


codeptr :: ByteString -> IO (Ptr a)
codeptr bs = do
  m <- mmap nullPtr (fromIntegral $ BS.length bs) 0x7 0x22 (-1) 0
  when (m == intPtrToPtr (-1))
    $ return (error "mmap failed")      -- TODO: use IO-based exceptions
  unsafeUseAsCStringLen bs \(p, l) -> copyBytes m p l
  return $ castPtr m


instance Functionable SymFn2 (FunPtr (F2 Double)) where
  fn Quot  = p_quot
  fn Rem   = p_fmod    -- FIXME: use fmod if we can
  fn Pow   = p_pow
  fn Upper = p_fmax
  fn Lower = p_fmin
  fn Atan2 = p_atan2

  -- NOTE: there's no reason to use these; they're just here for completeness.
  fn Add      = p_add
  fn Subtract = p_subtract
  fn Multiply = p_multiply
  fn Divide   = p_divide

instance Functionable SymFn2 (FunPtr (F2 Float)) where
  fn Quot  = p_quotf
  fn Rem   = p_fmodf    -- FIXME: use fmod if we can
  fn Pow   = p_powf
  fn Upper = p_fmaxf
  fn Lower = p_fminf
  fn Atan2 = p_atan2f

  -- NOTE: there's no reason to use these; they're just here for completeness.
  fn Add      = p_addf
  fn Subtract = p_subtractf
  fn Multiply = p_multiplyf
  fn Divide   = p_dividef


instance Functionable SymFn1 (FunPtr (F1 Double)) where
  fn Abs      = p_fabs
  fn Signum   = p_signum
  fn Log      = p_log
  fn Exp      = p_exp
  fn Sqrt     = p_sqrt
  fn Negate   = p_negate
  fn Sin      = p_sin
  fn Cos      = p_cos
  fn Tan      = p_tan
  fn Asin     = p_asin
  fn Acos     = p_acos
  fn Atan     = p_atan
  fn Sinh     = p_sinh
  fn Cosh     = p_cosh
  fn Tanh     = p_tanh
  fn Asinh    = p_asinh
  fn Acosh    = p_acosh
  fn Atanh    = p_atanh
  fn Ceiling  = p_ceil
  fn Floor    = p_floor
  fn Round    = p_round
  fn Truncate = p_truncate

instance Functionable SymFn1 (FunPtr (F1 Float)) where
  fn Abs      = p_fabsf
  fn Signum   = p_signumf
  fn Log      = p_logf
  fn Exp      = p_expf
  fn Sqrt     = p_sqrtf
  fn Negate   = p_negatef
  fn Sin      = p_sinf
  fn Cos      = p_cosf
  fn Tan      = p_tanf
  fn Asin     = p_asinf
  fn Acos     = p_acosf
  fn Atan     = p_atanf
  fn Sinh     = p_sinhf
  fn Cosh     = p_coshf
  fn Tanh     = p_tanhf
  fn Asinh    = p_asinhf
  fn Acosh    = p_acoshf
  fn Atanh    = p_atanhf
  fn Ceiling  = p_ceilf
  fn Floor    = p_floorf
  fn Round    = p_roundf
  fn Truncate = p_truncatef


-- Math function pointers available to JIT assemblers. These are
-- platform-independent.

type F1 r = r -> IO r
type F2 r = r -> r -> IO r


foreign import ccall "wrapper" fn1_dbl_p :: F1 Double -> IO (FunPtr (F1 Double))
foreign import ccall "wrapper" fn2_dbl_p :: F2 Double -> IO (FunPtr (F2 Double))

p_add      = unsafePerformIO $ fn2_dbl_p \x y -> return (x + y)
p_subtract = unsafePerformIO $ fn2_dbl_p \x y -> return (x - y)
p_multiply = unsafePerformIO $ fn2_dbl_p \x y -> return (x * y)
p_divide   = unsafePerformIO $ fn2_dbl_p \x y -> return (x / y)
p_quot     = unsafePerformIO $ fn2_dbl_p \x y -> return (x `quot` y)
p_rem      = unsafePerformIO $ fn2_dbl_p \x y -> return (x `rem` y)

p_signum   = unsafePerformIO $ fn1_dbl_p (return . signum)
p_truncate = unsafePerformIO $ fn1_dbl_p (return . truncate)
p_negate   = unsafePerformIO $ fn1_dbl_p (return . negate)


foreign import ccall "wrapper" fn1_float_p :: F1 Float  -> IO (FunPtr (F1 Float))
foreign import ccall "wrapper" fn2_float_p :: F2 Float  -> IO (FunPtr (F2 Float))

p_addf      = unsafePerformIO $ fn2_float_p \x y -> return (x + y)
p_subtractf = unsafePerformIO $ fn2_float_p \x y -> return (x - y)
p_multiplyf = unsafePerformIO $ fn2_float_p \x y -> return (x * y)
p_dividef   = unsafePerformIO $ fn2_float_p \x y -> return (x / y)
p_quotf     = unsafePerformIO $ fn2_float_p \x y -> return (x `quot` y)
p_remf      = unsafePerformIO $ fn2_float_p \x y -> return (x `rem` y)

p_signumf   = unsafePerformIO $ fn1_float_p (return . signum)
p_truncatef = unsafePerformIO $ fn1_float_p (return . truncate)
p_negatef   = unsafePerformIO $ fn1_float_p (return . negate)


foreign import ccall "math.h &pow"   p_pow   :: FunPtr (Double -> Double -> IO Double)
foreign import ccall "math.h &fmod"  p_fmod  :: FunPtr (Double -> Double -> IO Double)
foreign import ccall "math.h &fmax"  p_fmax  :: FunPtr (Double -> Double -> IO Double)
foreign import ccall "math.h &fmin"  p_fmin  :: FunPtr (Double -> Double -> IO Double)
foreign import ccall "math.h &atan2" p_atan2 :: FunPtr (Double -> Double -> IO Double)

foreign import ccall "math.h &powf"   p_powf   :: FunPtr (Float -> Float -> IO Float)
foreign import ccall "math.h &fmodf"  p_fmodf  :: FunPtr (Float -> Float -> IO Float)
foreign import ccall "math.h &fmaxf"  p_fmaxf  :: FunPtr (Float -> Float -> IO Float)
foreign import ccall "math.h &fminf"  p_fminf  :: FunPtr (Float -> Float -> IO Float)
foreign import ccall "math.h &atan2f" p_atan2f :: FunPtr (Float -> Float -> IO Float)

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
