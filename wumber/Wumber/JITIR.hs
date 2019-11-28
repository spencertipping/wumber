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
import Data.IntMap.Strict  (IntMap, (!), (!?), adjust, assocs, delete, elems,
                            empty, keys, insert, lookupMax, member, size)
import Data.List           (foldl', sortOn)
import Data.Maybe          (fromJust)
import Data.Tuple          (swap)
import Lens.Micro          ((&))
import Text.Printf         (printf)

import qualified Data.IntMap as IM
import qualified Data.Map    as M

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
--   This is used by scheduling functions like 'startable' and 'runnable' to
--   prioritize thread advancement.
--
--   NOTE: a graph may require an arbitrarily large number of registers to
--   evaluate, sometimes more than the processor can provide. In this case
--   'RegID' values will exceed the number of physical registers and the JIT
--   backend is expected to spill registers onto the stack or other memory.

data ThreadGraph a = TG { tg_ret :: !ThreadID,
                          tg_reg :: IntMap RegID,
                          tg_thr :: IntMap [Insn a] }
  deriving (Eq)

type ThreadID = Int
type RegID    = Int

instance Show a => Show (ThreadGraph a) where
  show (TG r tr tg) = concatMap format (keys tg)
    where format t = printf "% 3d % -4s % 3s %s\n"
                     t
                     (if t == r then "R" else " ")
                     (if member t tr then printf "r=%d" (tr ! t) else "")
                     (concatMap (printf "%-8s" . show) (tg ! t) :: String)


-- | A single transformation within a thread. 'LoadVal' and 'LoadVar' are used
--   only at the beginning of a thread to initialize the register.
data Insn a = LoadVal !a
            | LoadVar !VarID
            | I1      !SymFn1
            | I2      !SymFn2 !ThreadID
  deriving (Eq, Ord)

instance Show a => Show (Insn a) where
  show (LoadVal x) = "=" ++ show x
  show (LoadVar i) = "%" ++ show i
  show (I1 f)      = show f
  show (I2 f t)    = printf "%.3s(%d)" (show f) t


-- | Register read latency, used for scheduling purposes. This is relevant only
--   for out-of-order processors. If your processor serializes every
--   instruction, then this will always be zero.
type RegDelay = Double


-- | Takes a 'Sym' expression and reduces it to a thread graph. Every thread
--   begins with a 'Load' instruction.
thread :: SymConstraints f a => Sym f a -> ThreadGraph a
thread s = deduplicate $ TG ret empty g
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

    f <.> g  = fmap f . g
    thr is   = do id <- gets $ \m -> fromJust (fst <$> lookupMax m <|> Just (-1))
                  modify' $ insert (id + 1) is
                  return  $ id + 1
    t ++= is = do is' <- is
                  modify' (adjust (++ is') t)
                  return t


-- | Unifies threads that are guaranteed to produce the same results. This is
--   equivalent to common subexpression elimination.
deduplicate :: Ord a => ThreadGraph a -> ThreadGraph a
deduplicate tg@(TG r th g)
  | size th > 0      = error "can't deduplicate a running thread graph"
  | size g' < size g = deduplicate (TG r' th g')
  | otherwise        = tg
  where rindex = IM.toList g     & map swap & M.fromList
        g'     = M.toList rindex & map swap & map rewrite_all & IM.fromList
        r'     = reindex r

        rewrite_all (t, is) = (t, map rewrite is)
        rewrite (I2 f t)    = I2 f (reindex t)
        rewrite i           = i
        reindex t           = rindex M.! (g ! t)


-- | Returns the minimum number of /additional/ registers required to evaluate
--   the specified thread within the graph. Threads that are already bound to
--   registers don't count against the total.
--
--   The logic here is a bit subtle because we can choose when to start each
--   thread. For example, suppose we have something like this:
--
--   @
--   t1 : [LoadVal 0, I2 Add t2, I2 Add t3]
--   t2 : [...]   -- requires 6 registers
--   t3 : [...]   -- requires 3 registers
--   @
--
--   Technically we need only six registers to evaluate 't1' because we can
--   evaluate 't2' first, hold the result, and then start 't1' and 't3'.
--   However, the context of this function is "how many registers do we need to
--   evaluate 't1' assuming we start it now" -- so we would return seven.

required_registers :: ThreadGraph a -> ThreadID -> Int
required_registers tg@(TG _ tr g) t = this + deps
  where this = if member t tr then 0 else 1
        deps = foldl' max 0
               $ map (required_registers tg)
               $ thread_dependencies (g ! t)


-- | Returns a list of threads upon whose results the specified instructions
--   depend.
thread_dependencies :: [Insn a] -> [ThreadID]
thread_dependencies is = concatMap deps is
  where deps (I2 _ t) = [t]
        deps _        = []


-- | Returns an ordered list of threads that can be started. The list is ordered
--   by scheduling preference: if you have /n/ registers available, you should
--   start the first /n/ or /n - 1/ threads in the list. This should jointly
--   minimize the time spent in 'startable', and the time registers spend pinned
--   to return values -- although this function only approximates the optimal
--   solution because I'm a millennial.
--
--   The basic idea is that we want to start threads that either allow running
--   threads to progress or consume return values from threads that will be done
--   soon. In each case we want to avoid spilling registers by making sure that
--   the set of running threads will produce instructions that reduce the number
--   of register pins as they progress -- although this isn't always possible.
--
--   Structurally, we want something similar to depth-first with limited fanout.
--   We can get a lot of this by starting with the thread we want to return,
--   then descending into dependencies until we have enough registers to start
--   those threads. I doubt this is the right long-term solution, but it's
--   simple and should do what we want for now.
--
--   TODO: optimize

startable :: Int -> ThreadGraph a -> [ThreadID]
startable nregs tg@(TG r tr g) = go r
  where free = nregs - size tr
        go t | required_registers tg t < free = [t]
             | otherwise = concatMap go (thread_dependencies (g ! t))


-- | Returns threads whose next instructions can be executed, sorted by
--   increasing register access latency. Generally, a JIT backend should try to
--   advance every thread whose 'RegDelay' is zero before calling 'runnable'
--   again. This should minimize register latencies and time spent in the
--   scheduler.

runnable :: (RegID -> RegDelay) -> ThreadGraph a -> [(ThreadID, RegDelay)]
runnable rd (TG _ tr tg) = sortOn snd $ map (adjoin latency) running
  where
    adjoin f x  = (x, f x)
    running     = filter steppable $ filter (flip member tr) $ keys tg

    steppable t = case tg ! t of
      []            -> False
      LoadVal _ : _ -> True
      LoadVar _ : _ -> True
      I1 _      : _ -> True
      I2 _ t'   : _ -> null (tg ! t') && member t' tr

    latency t = max (rd (tr ! t)) $ case tg ! t of
      []            -> error "finished thread has no latency (internal error)"
      LoadVal _ : _ -> 0
      LoadVar _ : _ -> 0
      I1 _      : _ -> 0
      I2 _ t'   : _ -> rd (tr ! t')
