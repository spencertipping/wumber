{-# LANGUAGE BlockArguments #-}

{-# OPTIONS_GHC -funbox-strict-fields -Wincomplete-patterns #-}

-- | Intermediate representation for JITable expressions. This is the bulk of
--   the compile step for 'Sym' trees; after this, backend modules like
--   'AMD64Asm' take over and emit machine code.
--
--   The premise for this module is pretty straightforward. Most of the
--   scheduling logic is in the JIT backend, so our job is to reduce 'Sym' trees
--   down to a deduplicated, topsorted graph that can be executed incrementally.

module Wumber.JITIR where


import Data.IntMap     (IntMap)
import Data.List       (groupBy, sort)
import Data.Map.Strict (Map)

import Wumber.Fingerprint
import Wumber.MathFn
import Wumber.SymExpr
import Wumber.SymMath

import qualified Data.IntMap   as IM
import qualified Data.Map      as M
import qualified Wumber.BitSet as BS


-- TODO
-- Have a way to track which values are ready to be used?

data IR f a = IR { _ir_ret  :: IRID,
                   _ir_done :: BS.BitSet,
                   _ir_ops  :: IntMap (IROperand f a) }
  deriving (Show, Eq, Ord)

data IROperand f a = IRC !a
                   | IRV !VarID
                   | IRF f [IRID]
  deriving (Show, Eq, Ord)

type IRID = Int


compile_ir :: (Fingerprintable f, Fingerprintable a) => Sym p f a -> IR f a
compile_ir v = IR ret BS.empty ops
  where table = M.fromList [(fingerprint x, x) | x <- descendants v]
        index = M.fromList $ zip (M.keys table) [0..]
        ret   = index M.! fingerprint v
        ops   = IM.fromList [(index M.! fingerprint v, op v) | v <- M.elems table]

        op (SymC x)      = IRC x
        op (SymV i)      = IRV i
        op (SymF f xs _) = IRF f [index M.! fingerprint x | x <- xs]
