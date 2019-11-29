{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | Platform-independent JIT compilation for 'Sym' expressions. If you have a
--   'Sym' and want to make it fast without knowing the details, this is the
--   module for you.
--
--   If you aren't on a platform we support JIT for, then your 'Sym' expressions
--   will still run but they'll be interpreted-speed.
--
--   TODO: provide a bytecode interpreter to make the fallback case less awful

module Wumber.SymbolicJIT (
  is_jit_supported,
  jit_as_machinecode,
  jit
) where


import System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString      as BS
import qualified Data.Vector.Storable as VS


#if __amd64__ || __amd64 || __x86_64__ || __x86_64
#  define WUMBER_ARCH amd64
#else
#  define WUMBER_ARCH unknown
#endif

#if WUMBER_ARCH == amd64
import Wumber.AMD64JIT
#endif

import Wumber.JIT
import Wumber.JITIR
import Wumber.Numeric
import Wumber.Symbolic


-- | Takes a 'Sym' expression and returns a function that evaluates it at a
--   given 'Arg' state vector. In general, 'jit sym v == eval (v !) sym', but
--   'jit' will usually be faster if JIT is supported for your platform. You can
--   use 'is_jit_supported' to determine this.
jit :: FConstraints f R => Sym f R -> VS.Vector R -> R


-- | Compiles a 'Sym' expression to machine code, but doesn't coerce that code
--   to a function pointer.
jit_as_machinecode :: FConstraints f R => Sym f R -> BS.ByteString


-- | Returns 'True' if we support JIT on the platform being compiled. You can
--   still use 'jit' on unsupported platforms, but it will back into 'eval' and
--   be much slower.
is_jit_supported :: Bool


#if WUMBER_ARCH == amd64

is_jit_supported = True

jit_as_machinecode sym = assemble_graph (PM 10) $ thread sym

jit sym = unsafePerformIO do
  f <- compile_machinecode dblfn (jit_as_machinecode sym)
  return \v -> unsafePerformIO (VS.unsafeWith v f)

#else

is_jit_supported     = False
jit_as_machinecode s = error "JIT compilation unsupported on this platform"
jit sym v            = eval (v VS.!) sym

#endif
