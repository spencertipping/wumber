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
  jit
) where


import System.IO.Unsafe (unsafePerformIO)
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

-- | Returns 'True' if we support JIT on the platform being compiled. You can
--   still use 'jit' on unsupported platforms, but it will back into 'eval' and
--   be much slower.
is_jit_supported :: Bool


#if WUMBER_ARCH == amd64

is_jit_supported = True
jit sym = unsafePerformIO do
  let asm = assemble_graph (PM 10) $ thread sym
  f <- compile_machinecode dblfn asm
  return $ unsafePerformIO . flip VS.unsafeWith f

#else

is_jit_supported = False
jit sym v = eval (v VS.!) sym

#endif
