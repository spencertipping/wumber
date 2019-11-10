{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | A DSL for assembling machine code and doing requisite value conversions.
--   Assemblers are similar to 'ByteString' 'Builder's, but monadic (i.e.
--   non-associative) so they can track byte offsets.

module Wumber.Assembler where


import Control.Monad.RWS (RWS, evalRWS, get, modify, tell)
import Data.String       (IsString(..))

import qualified Data.ByteString         as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy    as BL


-- | Assembler state, which is currently just the byte offset from the beginning
--   of code.
type AsmState = Int
type Asm      = RWS () B.Builder AsmState


-- | Assembles code, returning a strict 'ByteString' of the result. You can use
--   this directly with 'with_jit' or 'compile' from 'Wumber.JIT'.
assemble :: Asm a -> BS.ByteString
assemble m = BL.toStrict $ B.toLazyByteString b where b = snd $ evalRWS m () 0


label :: Asm Int
label = get

emit :: B.Builder -> Asm ()
emit b = do
  tell b
  modify (+ fromIntegral (BL.length $ B.toLazyByteString b))

-- TODO: actual DSL stuff (once I figure out what I want it to be)
