{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}

{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | Platform-independent JIT compilation for 'Sym' expressions. If you have a
--   'Sym' and want to make it fast without knowing the details, this is the
--   module for you.
--
--   If you aren't on a platform we support JIT for, then your 'Sym' expressions
--   will still run but they'll be interpreted-speed.
--
--   TODO: provide a bytecode interpreter to make the fallback case less awful

module Wumber.SymJIT (
  is_jit_supported,
  jit_as_machinecode,
  jit
) where


import System.IO.Unsafe (unsafePerformIO)

import Wumber.VectorConversion

import qualified Data.ByteString      as BS
import qualified Data.Vector.Storable as VS


#define WUMBER_UNKNOWN 0
#define WUMBER_AMD64 1


#if __amd64__ || __amd64 || __x86_64__ || __x86_64
-- #  define WUMBER_ARCH WUMBER_AMD64
#  define WUMBER_ARCH WUMBER_UNKNOWN
#else
#  define WUMBER_ARCH WUMBER_UNKNOWN
#endif

#if WUMBER_ARCH == WUMBER_AMD64
import Wumber.AMD64JIT
#endif

#if WUMBER_ARCH != WUMBER_UNKNOWN
import Wumber.JIT
import Wumber.JITIR
#endif

import Wumber.Fingerprint
import Wumber.MathFn
import Wumber.Numeric
import Wumber.SymExpr
import Wumber.SymMath


-- | Takes a 'Sym' expression and returns a function that evaluates it at a
--   given 'Arg' state vector. In general, 'jit sym v == eval (v !) sym', but
--   'jit' will usually be faster if JIT is supported for your platform. You can
--   use 'is_jit_supported' to determine this.
jit :: (SymMathC f R, Fingerprintable f, VectorConversion v (VS.Vector R))
    => SymMath f R -> v -> R


-- | Compiles a 'Sym' expression to machine code, but doesn't coerce that code
--   to a function pointer.
jit_as_machinecode :: (SymMathC f R, Fingerprintable f)
                   => SymMath f R -> BS.ByteString


-- | Returns 'True' if we support JIT on the platform being compiled. You can
--   still use 'jit' on unsupported platforms, but it will back into 'eval' and
--   be much slower.
is_jit_supported :: Bool


#if WUMBER_ARCH == WUMBER_AMD64

is_jit_supported = True

jit_as_machinecode = assemble_ir . compile_ir . unMath

jit sym = unsafePerformIO do
  f <- compile_machinecode dblfn (jit_as_machinecode sym)
  return \v -> unsafePerformIO (VS.unsafeWith (vconvert v :: VS.Vector R) f)

#else

is_jit_supported     = False
jit_as_machinecode s = error "JIT compilation unsupported on this platform"
jit sym v            = eval (vconvert v VS.!) sym

#endif
