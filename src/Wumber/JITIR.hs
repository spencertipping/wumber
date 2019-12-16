{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -funbox-strict-fields -Wincomplete-patterns #-}

-- | Intermediate representation for JITable expressions. This is the bulk of
--   the compile step for 'Sym' trees; after this, backend modules like
--   'AMD64Asm' take over and emit machine code.
--
--   The premise for this module is pretty straightforward. Most of the
--   scheduling logic is in the JIT backend, so our job is to reduce 'Sym' trees
--   down to a deduplicated graph that can be executed incrementally.

module Wumber.JITIR where


import Data.Binary     (Binary)
import Data.Foldable   (toList)
import Data.IntMap     (IntMap)
import Data.List       (groupBy, sort)
import Data.Map.Strict (Map)
import GHC.Generics    (Generic)
import Lens.Micro      ((%~))
import Lens.Micro.TH   (makeLenses)

import Wumber.Fingerprint
import Wumber.MathFn
import Wumber.SymExpr
import Wumber.SymMath

import qualified Data.IntMap   as IM
import qualified Data.Map      as M
import qualified Wumber.BitSet as BS


-- | A backend-agnostic SSA-style graph that can be consumed by a JIT backend.
--   Any control flow behavior is delegated to the function type @f@, and we
--   don't do much with it.
--
--   IR represents a function that can return more than one value. Typically
--   this is done by destructively writing entries into a vector designated as
--   return-value storage. This pattern works well for derivative/gradient
--   functions, for example.
--
--   Functions like 'IfNN' make it so that not every operand is always required.
--   Although the exact details of which values are needed is beyond the scope
--   of this code, we nonetheless can control which values we want to compute by
--   setting bits in '_ir_req' ("requested values"). JIT backends can assemble
--   multiple branches, each with a different 'IR' state, before merging them
--   later on.

data IR f a = IR { _ir_ret  :: IRID,
                   _ir_rv   :: [IRID],
                   _ir_req  :: BS.BitSet,
                   _ir_done :: BS.BitSet,
                   _ir_ops  :: IntMap (IROperand f a) }
  deriving (Show, Eq, Ord, Generic, Binary)

data IROperand f a = IRC !a
                   | IRV !VarID
                   | IRF f [IRID]
  deriving (Show, Read, Eq, Ord, Generic, Binary)

type IRID = Int

makeLenses ''IR


-- | Compiles one or more symbolic quantities to a shared IR graph. In general,
--   if you have /n/ different functions to compute, you should compile them
--   together rather than separately because the IR deduplicates subexpressions.
compile_ir :: (Foldable t, Fingerprintable f, Fingerprintable a)
           => t (Sym p f a) -> IR f a
compile_ir v = IR r rs (BS.fromList (r:rs)) BS.empty ops
  where syms  = toList v
        table = M.fromList [(fingerprint x, x) | s <- syms, x <- descendants s]
        index = M.fromList $ zip (M.keys table) [0..]
        r:rs  = [index M.! fingerprint s | s <- syms]
        ops   = IM.fromList
                [(index M.! fingerprint v, op v) | v <- M.elems table]

        op (SymC x)      = IRC x
        op (SymV i)      = IRV i
        op (SymF f xs _) = IRF f [index M.! fingerprint x | x <- xs]


-- | Returns a lazy list of ops whose operands are ready to be run, and whose
--   results have not already been produced. Only requested values will be
--   returned; you'll need to call 'ir_request' to make them schedulable.
ir_runnable :: IR f a -> [IRID]
ir_runnable (IR _ _ r d o) = [k | (k, v) <- IM.toList o,
                                  BS.member k r,
                                  all (flip BS.member d) (op_deps v)]


-- | Requests that a set of values be calculated. Operands of the values are
--   /not/ automatically requested because the IR layer doesn't know which ones
--   are necessary to evaluate the function.
ir_request :: [IRID] -> IR f a -> IR f a
ir_request b = ir_req %~ BS.union (BS.fromList b)


-- | Dependencies for the specified operand.
op_deps :: IROperand f a -> [IRID]
op_deps (IRF _ xs) = xs
op_deps _          = []
