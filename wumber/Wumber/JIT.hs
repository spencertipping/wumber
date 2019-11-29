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
import System.IO              (hPutStrLn, stderr)
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


slow_fn name fio = unsafePerformIO do
  hPutStrLn stderr $ "WARNING: using slow haskell variant " ++ name
  fio


foreign import ccall "wrapper" fn1_dbl_p :: F1 Double -> IO (FunPtr (F1 Double))
foreign import ccall "wrapper" fn2_dbl_p :: F2 Double -> IO (FunPtr (F2 Double))

p_add      = slow_fn "add"  $ fn2_dbl_p \x y -> return (x + y)
p_subtract = slow_fn "sub"  $ fn2_dbl_p \x y -> return (x - y)
p_multiply = slow_fn "mul"  $ fn2_dbl_p \x y -> return (x * y)
p_divide   = slow_fn "div"  $ fn2_dbl_p \x y -> return (x / y)
p_quot     = slow_fn "quot" $ fn2_dbl_p \x y -> return (x `quot` y)
p_rem      = slow_fn "rem"  $ fn2_dbl_p \x y -> return (x `rem` y)

p_signum   = slow_fn "sign"  $ fn1_dbl_p (return . signum)
p_truncate = slow_fn "trunc" $ fn1_dbl_p (return . truncate)
p_negate   = slow_fn "neg"   $ fn1_dbl_p (return . negate)


foreign import ccall "wrapper" fn1_float_p :: F1 Float -> IO (FunPtr (F1 Float))
foreign import ccall "wrapper" fn2_float_p :: F2 Float -> IO (FunPtr (F2 Float))

p_addf      = slow_fn "addf"  $ fn2_float_p \x y -> return (x + y)
p_subtractf = slow_fn "subf"  $ fn2_float_p \x y -> return (x - y)
p_multiplyf = slow_fn "mulf"  $ fn2_float_p \x y -> return (x * y)
p_dividef   = slow_fn "divf"  $ fn2_float_p \x y -> return (x / y)
p_quotf     = slow_fn "quotf" $ fn2_float_p \x y -> return (x `quot` y)
p_remf      = slow_fn "rem"   $ fn2_float_p \x y -> return (x `rem` y)

p_signumf   = slow_fn "signf"  $ fn1_float_p (return . signum)
p_truncatef = slow_fn "truncf" $ fn1_float_p (return . truncate)
p_negatef   = slow_fn "negf"   $ fn1_float_p (return . negate)


foreign import ccall unsafe "math.h &pow"   p_pow   :: FunPtr (F2 Double)
foreign import ccall unsafe "math.h &fmod"  p_fmod  :: FunPtr (F2 Double)
foreign import ccall unsafe "math.h &fmax"  p_fmax  :: FunPtr (F2 Double)
foreign import ccall unsafe "math.h &fmin"  p_fmin  :: FunPtr (F2 Double)
foreign import ccall unsafe "math.h &atan2" p_atan2 :: FunPtr (F2 Double)

foreign import ccall unsafe "math.h &powf"   p_powf   :: FunPtr (F2 Float)
foreign import ccall unsafe "math.h &fmodf"  p_fmodf  :: FunPtr (F2 Float)
foreign import ccall unsafe "math.h &fmaxf"  p_fmaxf  :: FunPtr (F2 Float)
foreign import ccall unsafe "math.h &fminf"  p_fminf  :: FunPtr (F2 Float)
foreign import ccall unsafe "math.h &atan2f" p_atan2f :: FunPtr (F2 Float)

foreign import ccall unsafe "math.h &fabs"  p_fabs  :: FunPtr (F1 Double)
foreign import ccall unsafe "math.h &log"   p_log   :: FunPtr (F1 Double)
foreign import ccall unsafe "math.h &exp"   p_exp   :: FunPtr (F1 Double)
foreign import ccall unsafe "math.h &sqrt"  p_sqrt  :: FunPtr (F1 Double)
foreign import ccall unsafe "math.h &sin"   p_sin   :: FunPtr (F1 Double)
foreign import ccall unsafe "math.h &cos"   p_cos   :: FunPtr (F1 Double)
foreign import ccall unsafe "math.h &tan"   p_tan   :: FunPtr (F1 Double)
foreign import ccall unsafe "math.h &asin"  p_asin  :: FunPtr (F1 Double)
foreign import ccall unsafe "math.h &acos"  p_acos  :: FunPtr (F1 Double)
foreign import ccall unsafe "math.h &atan"  p_atan  :: FunPtr (F1 Double)
foreign import ccall unsafe "math.h &sinh"  p_sinh  :: FunPtr (F1 Double)
foreign import ccall unsafe "math.h &cosh"  p_cosh  :: FunPtr (F1 Double)
foreign import ccall unsafe "math.h &tanh"  p_tanh  :: FunPtr (F1 Double)
foreign import ccall unsafe "math.h &asinh" p_asinh :: FunPtr (F1 Double)
foreign import ccall unsafe "math.h &acosh" p_acosh :: FunPtr (F1 Double)
foreign import ccall unsafe "math.h &atanh" p_atanh :: FunPtr (F1 Double)
foreign import ccall unsafe "math.h &ceil"  p_ceil  :: FunPtr (F1 Double)
foreign import ccall unsafe "math.h &floor" p_floor :: FunPtr (F1 Double)
foreign import ccall unsafe "math.h &round" p_round :: FunPtr (F1 Double)

foreign import ccall unsafe "math.h &fabsf"  p_fabsf  :: FunPtr (F1 Float)
foreign import ccall unsafe "math.h &logf"   p_logf   :: FunPtr (F1 Float)
foreign import ccall unsafe "math.h &expf"   p_expf   :: FunPtr (F1 Float)
foreign import ccall unsafe "math.h &sqrtf"  p_sqrtf  :: FunPtr (F1 Float)
foreign import ccall unsafe "math.h &sinf"   p_sinf   :: FunPtr (F1 Float)
foreign import ccall unsafe "math.h &cosf"   p_cosf   :: FunPtr (F1 Float)
foreign import ccall unsafe "math.h &tanf"   p_tanf   :: FunPtr (F1 Float)
foreign import ccall unsafe "math.h &asinf"  p_asinf  :: FunPtr (F1 Float)
foreign import ccall unsafe "math.h &acosf"  p_acosf  :: FunPtr (F1 Float)
foreign import ccall unsafe "math.h &atanf"  p_atanf  :: FunPtr (F1 Float)
foreign import ccall unsafe "math.h &sinhf"  p_sinhf  :: FunPtr (F1 Float)
foreign import ccall unsafe "math.h &coshf"  p_coshf  :: FunPtr (F1 Float)
foreign import ccall unsafe "math.h &tanhf"  p_tanhf  :: FunPtr (F1 Float)
foreign import ccall unsafe "math.h &asinhf" p_asinhf :: FunPtr (F1 Float)
foreign import ccall unsafe "math.h &acoshf" p_acoshf :: FunPtr (F1 Float)
foreign import ccall unsafe "math.h &atanhf" p_atanhf :: FunPtr (F1 Float)
foreign import ccall unsafe "math.h &ceilf"  p_ceilf  :: FunPtr (F1 Float)
foreign import ccall unsafe "math.h &floorf" p_floorf :: FunPtr (F1 Float)
foreign import ccall unsafe "math.h &roundf" p_roundf :: FunPtr (F1 Float)
