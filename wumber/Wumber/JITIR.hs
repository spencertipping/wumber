{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -funbox-strict-fields -Wincomplete-patterns #-}

-- | Intermediate representation for JITable expressions. This is the bulk of
--   the compile step for 'Sym' trees; after this, backend modules like
--   'AMD64Asm' take over and emit machine code.
--
--   I'm not worried about vectorization yet; a much bigger optimization is to
--   get full throughput from arithmetic operations by running multiple
--   independent subexpressions in parallel (by having them refer to disjoint
--   registers). There's a tradeoff: too few threads and we block on instruction
--   latencies, too many and we're IO-bound against L1 cache. We need to let the
--   JIT backend dictate how many threads it's running at any given moment.
--
--   So, what's a thread? It's a serial computation that accumulates into one
--   register and intermittently uses a second register for operands. In theory,
--   a processor with /n/ registers can run /n - 1/ threads without spilling.

module Wumber.JITIR (
  Thread(..),
  Insn(..),
  thread
) where


import Wumber.Symbolic


-- | A series of transformations to a single initial value.
data Thread a = Thr !(Either a VarID) [Insn a] deriving (Eq, Show)

-- | A single transformation within a thread.
data Insn a = I1  SymFn1
            | I2L SymFn2 !(Thread a)  -- Accumulated value is on the left
            | I2R SymFn2 !(Thread a)  -- Accumulated value is on the right
  deriving (Eq, Show)


-- | Constructs a thread graph from a 'Sym' object.
thread :: SymConstraints f a => Sym f a -> Thread a
thread s = case s of [t] :+ 0 -> tt t
                     ts  :+ n -> Thr (Left n) (map (I2L Add . tt) ts)
  where
    tt (1 :* [e]) = et e
    tt (a :* es)  = Thr (Left a) (map (I2L Multiply . et) es)
    et (x :** 1)  = vt x
    et (x :** n)  = Thr (Left n) [I2R Pow (vt x)]

    vt (Var i)                 = Thr (Right i) []
    vt (Fn1 f _ (OS a))        = Thr (Left 0) [I2R Add (thread a), I1 f]
    vt (Fn2 f _ (OS a) (OS b)) = Thr (Left 0) [I2R Add (thread a), I2L f (thread b)]
    vt (FnN _ _ _)             = error "FnN isn't supported in IR yet"
