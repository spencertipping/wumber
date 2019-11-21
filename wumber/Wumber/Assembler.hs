{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | A DSL for assembling machine code and doing requisite value conversions.
--   Assemblers are similar to 'ByteString' 'Builder's, but monadic (i.e.
--   non-associative) so they can do things like track byte offsets.
--
--   TODO: multipart assembly so we can refer to branch addresses
--   TODO: build up a constant vector

module Wumber.Assembler (
  assemble,
  Assembler,
  hex
) where


import Control.Monad.RWS (RWS, evalRWS, tell)
import Numeric           (readHex)

import qualified Data.ByteString         as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy    as BL


-- | Assembles code, returning a strict 'ByteString' of the result. You can use
--   this directly with 'compile' from 'Wumber.JIT'.
assemble :: Assembler r s a -> r -> s -> BS.ByteString
assemble m r s = BL.toStrict $ B.toLazyByteString $ snd $ evalRWS m r s


type Assembler r s = RWS r B.Builder s


hex :: String -> Assembler r s ()
hex []        = return ()
hex (d1:d2:s) = do tell $ B.word8 (fromIntegral h); hex s
  where [(h, _)] = readHex [d1, d2]
