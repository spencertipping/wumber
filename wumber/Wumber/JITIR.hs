{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

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
import Data.Map            (Map)
import Data.Maybe          (fromJust)
import Data.Tuple          (swap)
import GHC.Generics        (Generic(..), Generic1(..))
import Lens.Micro          ((&))
import Lens.Micro.TH       (makeLenses)
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

data ThreadGraph a = TG { _tg_ret :: !ThreadID,
                          _tg_thr :: IntMap [Insn a] }
  deriving (Eq, Generic, Generic1)

type ThreadID = Int


-- | A single transformation within a thread. 'LoadVal', 'LoadVar', and
--   'LoadThr' are used only at the beginning of a thread to set an initial
--   value.
--
--   TODO: add memory->register loads here; then the JIT backend won't have to
--   track residence or defensively unspill registers when allocating them.

data Insn a = LoadVal !a
            | LoadVar !VarID
            | LoadThr !ThreadID
            | I1      !SymFn1
            | I2      !SymFn2 !ThreadID
            | I2C     !SymFn2 !a
  deriving (Eq, Ord, Generic, Generic1)


data InsnProfile = PI1  !SymFn1
                 | PI2  !SymFn2
                 | PI2C !SymFn2
                 | PLoadVal
                 | PLoadVar
                 | PLoadThr
  deriving (Eq, Ord, Show, Generic)

newtype GraphProfile = GP (Map InsnProfile Int)


-- | Thread read latency, used for scheduling purposes. Usually threads will
--   have nonzero latency arising from two factors:
--
--   1. The register for the thread has been spilled to memory.
--   2. The thread exists in a register on a superscalar architecture, and
--      accessing the register would stall the instruction pipeline.
type Latency = Double


-- | Thread priority, used as a hint from functions like 'startable' to indicate
--   relative importance. Higher values are higher priority.
type Priority = Int


makeLenses ''ThreadGraph
makeLenses ''Insn


instance Show a => Show (ThreadGraph a) where
  show (TG r tg) = concatMap format (keys tg)
    where format t = printf "% 3d%s %s\n"
                     t
                     (if t == r then "R" else " " :: String)
                     (concatMap (printf "%-8s" . show) (tg ! t) :: String)

instance Show a => Show (Insn a) where
  show (LoadVal x) = "=" ++ show x
  show (LoadVar i) = "%" ++ show i
  show (LoadThr t) = "t" ++ show t
  show (I1 f)      = show f
  show (I2 f t)    = printf "%.3s(%d)" (show f) t
  show (I2C f x)   = printf "%.3s(%s)" (show f) (show x)

instance Show GraphProfile where
  show (GP m) = concatMap format (M.assocs m)
    where format (p, c) = printf "% 4d  %s\n" c (show p)


-- | Returns the number of threads referred to by a thread graph. This is an
--   upper bound on the number of local variable slots you should allocate.
n_threads :: ThreadGraph a -> Int
n_threads (TG _ g) = size g


-- | Determines whether a thread is done running.
complete :: ThreadGraph a -> ThreadID -> Bool
complete (TG _ g) t = null (g ! t)


-- | Determines whether a thread is currently blocked on an unavailable value.
blocked :: ThreadGraph a -> ThreadID -> Bool
blocked tg@(TG _ g) t = b (g ! t)
  where b (I2 _ t' : _) = not $ complete tg t'
        b _             = False


-- | Steps the graph, pulling the next instruction for the specified thread. The
--   thread must not be 'complete'.
thread_step :: ThreadGraph a -> ThreadID -> (Insn a, ThreadGraph a)
thread_step (TG r g) t = (head (g ! t), TG r (adjust tail t g))


-- TODO
-- Lift the latency model into a form we can use here, then explore the "within
-- N time steps" frontier for register load dependencies.


-- TODO
-- 'thread_unstep'
--
-- Is there a way to generalize this and thread_step? Ideally so we can operate
-- on multiple threads at once.


-- | Profiles a graph by measuring how many times it invokes each 'SymFn'
--   variant.
profile_graph :: ThreadGraph a -> GraphProfile
profile_graph (TG _ g) = GP $ M.unionsWith (+) $ map (flip M.singleton 1 . p)
                                               $ concat (elems g)
  where p (LoadVal _) = PLoadVal
        p (LoadVar _) = PLoadVar
        p (LoadThr _) = PLoadThr
        p (I1 f)      = PI1 f
        p (I2 f _)    = PI2 f
        p (I2C f _)   = PI2C f


-- | Takes a 'Sym' expression and reduces it to a thread graph. Every thread
--   begins with a 'Load' instruction. The thread graph we return will be
--   deduplicated, but will have a dense set of thread IDs. This means thread
--   IDs can be mapped directly to local variable offsets in an 'alloca' region.
--
--   TODO: index common subexpressions and assign them to threads. The example
--   iso model has duplicated work like this:
--
--   @
--   -1.0·Pow(x² + y² + z², 0.5)
--   -1.0·Pow(-1.8·x + x² + -1.8·y + y² + -1.8·z + z² + 2.43, 0.5)
--   @

thread :: SymConstraints f a => Sym f a -> ThreadGraph a
thread s = TG ret g & deduplicate
  where
    (ret, g) = runState (pt s) empty

    pt ([t] :+ 0) = tt t
    pt (ts  :+ n) = thr [LoadVal n] >>= (++= mapM (I2 Add <.> tt) ts)

    tt (1    :* [e]) = et e
    tt ((-1) :* [e]) = et e >>= (++= return [I1 Negate])
    tt (a    :* es)  = thr [LoadVal a] >>= (++= mapM (I2 Multiply <.> et) es)

    et (v :** 1)   = vt v
    et (v :** 0.5) = vt v >>= (++= return [I1 Sqrt])
    et (v :** n)   = do t <- vt v
                        t ++= return [I2C Pow n]

    vt (Var i)                         = thr [LoadVar i]
    vt (Poly (OS v))                   = pt v
    vt (Fn1 f _ (OS a))                = pt a >>= (++= return [I1 f])
    vt (Fn2 f _ (OS a) (OS ([] :+ b))) = do at <- pt a
                                            at ++= return [I2C f b]
    vt (Fn2 f _ (OS a) (OS b))         = do at <- pt a
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


-- | Unifies threads that are guaranteed to produce the same results. This is a
--   weak form of common subexpression elimination ("weak" because we don't also
--   find shared instruction prefixes, even though every thread is a pure
--   computation).
--
--   The graph is compacted so there are no holes in the space of thread IDs.
--
--   TODO: factor common prefixes into shared threads, then use 'LoadThr'

deduplicate :: Ord a => ThreadGraph a -> ThreadGraph a
deduplicate tg@(TG r g) | size g' < size g = deduplicate (TG r' g')
                        | otherwise        = TG (k' ! r) compact

  where ri = assocs g & map swap & M.fromList
        g' = M.toList ri & map swap & IM.fromList & IM.map (rewrite_all reindex)
        r' = reindex r

        k'      = zip (keys g) [0..] & IM.fromList
        compact = IM.assocs g & map (\(k, is) -> (k' ! k, rewrite_all (k' !) is))
                              & IM.fromList

        rewrite_all m is = map (rewrite m) is
        reindex t        = ri M.! (g ! t)

        rewrite m (I2 f t) = I2 f (m t)
        rewrite _ i        = i


-- | Advises the backend about which threads should be started. At any given
--   moment, we want to start threads that can either complete or do a lot of
--   work before blocking on a dependency.
--
--   The 'cbias' parameter specifies the priority associated with finishing a
--   thread. Higher values will cause 'startable' to focus more on threads that
--   can run to completion without blocking on dependencies, which should free
--   up other threads. The tradeoff is that doing too much of this may cause
--   unnecessary register shuffling. (TODO: is this true?)

startable :: Priority -> ThreadGraph a -> [(ThreadID, Priority)]
startable cbias tg@(TG _ g) = assocs g & filter incomplete
                                       & map priority & sortOn (negate . snd)
  where priority (t, is) = (t, nonblocking_insns is)
        block            = not . complete tg

        incomplete (_, []) = False
        incomplete _       = True

        nonblocking_insns (I2 _ t' : _) | block t' = 0
        nonblocking_insns []                       = cbias
        nonblocking_insns (i:is)                   = 1 + nonblocking_insns is


-- | Returns threads whose next instructions can be executed, sorted by
--   increasing register access latency. Generally, a JIT backend should try to
--   advance every thread whose 'Latency' is zero before calling 'runnable'
--   again.
--
--   Sometimes 'runnable' will provide no zero-latency threads, e.g. if
--   registers have been spilled to memory. In that case you should step the
--   first few threads and accept that there will be pipeline latency.

runnable :: (ThreadID -> Latency) -> ThreadGraph a -> [(ThreadID, Latency)]
runnable rd (TG _ g) = keys g & filter steppable
                              & map (\t -> (t, latency t))
                              & sortOn snd
  where
    steppable t = case g ! t of
      []             -> False
      LoadVal _  : _ -> True
      LoadVar _  : _ -> True
      LoadThr t' : _ -> null (g ! t')
      I1 _       : _ -> True
      I2C _ _    : _ -> True
      I2 _ t'    : _ -> null (g ! t')

    latency t = max (rd t) $ case g ! t of
      []             -> error "finished thread can't be stepped (internal error)"
      LoadVal _  : _ -> 0
      LoadVar _  : _ -> 0
      LoadThr t' : _ -> rd t'
      I1 _       : _ -> 0
      I2C _ _    : _ -> 0
      I2 _ t'    : _ -> rd t'
