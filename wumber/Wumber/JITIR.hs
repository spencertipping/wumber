{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
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
import Control.Monad.State (State, gets, modify', runState)
import Data.Map.Strict     (Map, (!), (!?), adjust, delete, empty, insert,
                            lookupMax, update)
import Data.Maybe          (fromJust)

import Wumber.Symbolic


-- | 'ThreadGraph' is an incremental data structure that keeps track of the
--   current register assignment for each thread, as well as the remaining
--   instruction queue for that thread. Any thread with an empty instruction
--   queue is removed from the graph.
--
--   'tg_ret' indicates which thread contains the value we should return when no
--   threads have instructions left to execute.
--
--   'tg_reg' contains mappings only for threads that have register allocations.

data ThreadGraph a = TG { tg_ret :: !ThreadID,
                          tg_reg :: Map ThreadID RegID,
                          tg_thr :: Map ThreadID [Insn a] }
  deriving (Show, Eq)

type ThreadID = Int
type RegID    = Int

-- | A single transformation within a thread. 'LoadVal' and 'LoadVar' are used
--   only at the beginning of a thread to initialize the register.
data Insn a = LoadVal !a
            | LoadVar !VarID
            | I1      !SymFn1
            | I2      !SymFn2 !ThreadID
  deriving (Eq, Show)


thread :: SymConstraints f a => Sym f a -> ThreadGraph a
thread s = TG ret empty g
  where
    (ret, g) = runState (pt s) empty

    pt ([t] :+ 0) = tt t
    pt ([t] :+ n) = tt t            >>= (++= return [LoadVal n])
    pt (ts  :+ n) = thr [LoadVal n] >>= (++= mapM (I2 Add <.> tt) ts)

    tt (1    :* [e]) = et e
    tt ((-1) :* [e]) = et e >>= (++= return [I1 Negate])
    tt (a    :* es)  = thr [LoadVal a] >>= (++= mapM (I2 Multiply <.> et) es)

    et (v :** 1)   = vt v
    et (v :** 0.5) = vt v >>= (++= return [I1 Sqrt])
    et (v :** n)   = do t <- vt v
                        e <- thr [LoadVal n]
                        t ++= return [I2 Pow e]

    vt (Var i)                 = thr [LoadVar i]
    vt (Fn1 f _ (OS a))        = pt a >>= (++= return [I1 f])
    vt (Fn2 f _ (OS a) (OS b)) = do at <- pt a
                                    bt <- pt b
                                    at ++= return [I2 f bt]
    -- TODO: FnN

    thr is   = do id <- gets $ \m -> fromJust (fst <$> lookupMax m <|> Just (-1))
                  modify' $ insert (id + 1) is
                  return  $ id + 1
    f <.> g  = fmap f . g
    t ++= is = do is' <- is
                  modify' (adjust (++ is') t)
                  return t
