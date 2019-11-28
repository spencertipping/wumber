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
import Data.List           (foldl', groupBy, inits, sortOn)
import Data.Maybe          (fromJust)
import Data.Tuple          (swap)
import Lens.Micro          ((&))
import Text.Printf         (printf)

import qualified Data.IntMap as IM
import qualified Data.Map    as M

import Wumber.Symbolic


-- | 'ThreadGraph' is an incremental data structure that keeps track of the
--   remaining instruction queue for that thread. Any thread with an empty
--   instruction queue is considered "done" and its result is usable by other
--   threads.
--
--   'tg_ret' indicates which thread contains the value we should return when no
--   threads have instructions left to execute.

data ThreadGraph a = TG { tg_ret :: !ThreadID,
                          tg_thr :: IntMap [Insn a] }
  deriving (Eq)

type ThreadID = Int

instance Show a => Show (ThreadGraph a) where
  show (TG r tg) = concatMap format (keys tg)
    where format t = printf "% 3d %s %s\n"
                     t
                     (if t == r then "R" else " ")
                     (concatMap (printf "%-8s" . show) (tg ! t) :: String)


-- | A single transformation within a thread. 'LoadVal', 'LoadVar', and
--   'LoadThr' are used only at the beginning of a thread to set an initial
--   value.
data Insn a = LoadVal !a
            | LoadVar !VarID
            | LoadThr !ThreadID
            | I1      !SymFn1
            | I2      !SymFn2 !ThreadID
  deriving (Eq, Ord)

instance Show a => Show (Insn a) where
  show (LoadVal x) = "=" ++ show x
  show (LoadVar i) = "%" ++ show i
  show (LoadThr t) = "t" ++ show t
  show (I1 f)      = show f
  show (I2 f t)    = printf "%.3s(%d)" (show f) t


-- | Thread read latency, used for scheduling purposes. Usually threads will
--   have nonzero latency arising from two factors:
--
--   1. The register for the thread has been spilled to memory.
--   2. The thread exists in a register on a superscalar architecture, and
--      accessing the register would stall the instruction pipeline.

type Latency = Double


-- | Takes a 'Sym' expression and reduces it to a thread graph. Every thread
--   begins with a 'Load' instruction. The thread graph we return will be
--   deduplicated, which means the set of thread IDs may not be dense.

thread :: SymConstraints f a => Sym f a -> ThreadGraph a
thread s = TG ret g & split_prefixes & deduplicate
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


-- | Identifies instruction prefixes shared by multiple threads and splits them
--   into separate threads. This results in stronger common subexpression
--   elimination from 'deduplicate'.

split_prefixes :: ThreadGraph a -> ThreadGraph a
split_prefixes (TG r g) = TG r g'
  where g' = g


-- | Unifies threads that are guaranteed to produce the same results. This is a
--   weak form of common subexpression elimination ("weak" because we don't also
--   find shared instruction prefixes, even though every thread is a pure
--   computation).

deduplicate :: Ord a => ThreadGraph a -> ThreadGraph a
deduplicate tg@(TG r g)
  | size g' < size g = deduplicate (TG r' g')
  | otherwise        = tg

  where rindex = IM.toList g     & map swap & M.fromList
        g'     = M.toList rindex & map swap & map rewrite_all & IM.fromList
        r'     = reindex r

        rewrite_all (t, is) = (t, map rewrite is)
        rewrite (I2 f t)    = I2 f (reindex t)
        rewrite i           = i
        reindex t           = rindex M.! (g ! t)


-- | Returns threads whose next instructions can be executed, sorted by
--   increasing register access latency. Generally, a JIT backend should try to
--   advance every thread whose 'Latency' is zero before calling 'runnable'
--   again. This should minimize register latencies and time spent in the
--   scheduler.

runnable :: (ThreadID -> Latency) -> ThreadGraph a -> [(ThreadID, Latency)]
runnable rd (TG _ g) = keys g & filter steppable
                              & map (adjoin latency)
                              & sortOn snd
  where
    adjoin f x = (x, f x)

    steppable t = case g ! t of
      []             -> False
      LoadVal _  : _ -> True
      LoadVar _  : _ -> True
      LoadThr t' : _ -> null (g ! t')
      I1 _       : _ -> True
      I2 _ t'    : _ -> null (g ! t')

    latency t = max (rd t) $ case g ! t of
      []             -> error "finished thread can't be stepped (internal error)"
      LoadVal _  : _ -> 0
      LoadVar _  : _ -> 0
      LoadThr t' : _ -> rd t'
      I1 _       : _ -> 0
      I2 _ t'    : _ -> rd t'
