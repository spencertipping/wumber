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

module Wumber.JITIR where


import Control.Applicative ((<|>))
import Data.Vector         (Vector, (!))

import Wumber.Symbolic


-- | A series of transformations to a single initial value.
data Thread a = Thr !(ThreadSource a) [Insn a] deriving (Eq, Show)
data ThreadSource a = Val  !a
                    | Read !VarID
                    | Reg  !RegID
  deriving (Eq, Show)

type RegID = Int

-- | A single transformation within a thread.
data Insn a = Assign !(ThreadSource a)
            | I1     !SymFn1
            | I2     !SymFn2 !(Thread a)
  deriving (Eq, Show)


-- | Constructs a thread graph from a 'Sym' object.
thread :: SymConstraints f a => Sym f a -> Thread a
thread s = case s of
  [t] :+ 0 -> tt t
  [t] :+ n -> tt t $+ [I2 Add (Thr (Val n) [])]
  ts  :+ n -> Thr (Val n) (map (I2 Add . tt) ts)

  where tt (1 :* [e]) = et e
        tt (a :* [e]) = et e $+ [I2 Multiply (Thr (Val a) [])]
        tt (a :* es)  = Thr (Val a) (map (I2 Multiply . et) es)
        et (x :** 1)  = vt x
        et (x :** n)  = vt x $+ [I2 Pow (Thr (Val n) [])]

        vt (Var i)                 = Thr (Read i) []
        vt (Fn1 f _ (OS a))        = thread a $+ [I1 f]
        vt (Fn2 f _ (OS a) (OS b)) = thread a $+ [I2 f (thread b)]
        -- TODO: FnN

        Thr i xs $+ ys = Thr i (xs ++ ys)


thread_started :: Thread a -> Bool
thread_started (Thr (Reg _) _) = True
thread_started _               = False


-- | Returns the next step and continuation for a thread, or the location of the
--   data if the thread is done.
continuation :: RegID -> Thread a -> ThreadStep a
continuation r t@(Thr s is)
  | not (thread_started t) = Right (Reg r, Assign s, Thr (Reg r) is)
  | otherwise              = case is of i:is' -> Right (s, i, Thr (Reg r) is')
                                        _     -> Left s

type ThreadStep a = Either (ThreadSource a) (ThreadSource a, Insn a, Thread a)


-- | Schedules the next instruction given the specified register constraints.
--   The register index is indicated by the 'ThreadSource' in 'Step'.
--
--   'schedule' runs instructions whose operands are 'Ready' if possible. If no
--   such instructions exist, it will then try to start a new thread on a free
--   register. Failing that, it returns 'Nothing'.
--
--   When a new thread is started, we prefer threads whose instructions'
--   operands are available. This way we minimize the amount of time spent
--   holding onto a register value without being able to free it.

schedule :: Vector RegisterState -> Thread a -> Maybe (ThreadStep a, Thread a)
schedule rs t = schedule_ready rs t <|> schedule_new rs t

schedule_ready rs t@(Thr (Reg i) _) = case c of
  Left _                                                      -> Nothing
  Right (Reg r,  I1 f, t')                   | ready [r]      -> Just (c, t')
  Right (Reg r1, I2 f (Thr (Reg r2) []), t') | ready [r1, r2] -> Just (c, t')

  where c     = continuation i t
        ready = all (\r -> rs ! r == Ready)

schedule_new   _ _ = Nothing


-- | Describes the state of a register: 'Free' (the scheduler can allocate it),
--   'Busy' (it's running an instruction, and accessing it would cause a stall),
--   or 'Ready' (it contains a result).
data RegisterState = Free
                   | Busy !Double
                   | Ready
  deriving (Eq, Show)
