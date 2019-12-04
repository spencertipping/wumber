{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | A DSL for assembling machine code and doing requisite value conversions.
--   Assemblers are similar to 'ByteString' 'Builder's, but monadic (i.e.
--   non-associative) so they can do things like track byte offsets.
--
--   TODO: multipart assembly so we can refer to branch addresses
--   TODO: build up a constant vector

module Wumber.Assembler where


import Control.Monad.RWS (RWS, evalRWS, tell)
import Data.Bits         (Bits(..), FiniteBits(..), shiftL, (.|.), finiteBitSize)
import Numeric           (readHex)

import qualified Data.ByteString         as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy    as BL

import Wumber.Numeric


-- | Assembles code, returning a strict 'ByteString' of the result. You can use
--   this directly with 'compile' from 'Wumber.JIT'.
assemble :: Assembler r s a -> r -> s -> BS.ByteString
assemble m r s = BL.toStrict $ B.toLazyByteString $ snd $ evalRWS m r s


type Assembler r s = RWS r B.Builder s


hex :: String -> Assembler r s ()
hex []        = return ()
hex (d1:d2:s) = do tell $ B.word8 (fromIntegral h); hex s
  where [(h, _)] = readHex [d1, d2]


-- | A register set of some size.
--
--   TODO: make this less silly

newtype RegSet a = RS a deriving (Show, Eq)

regset :: (Integral a, Num b, FiniteBits b) => [a] -> RegSet b
regset = RS . flip foldl 0 setBit . map fi

unregset :: (Num a, FiniteBits b) => RegSet b -> [a]
unregset (RS s) = map fi $ filter (testBit s) [0 .. finiteBitSize s - 1]

regset_free :: (Num a, FiniteBits b) => RegSet b -> [a]
regset_free (RS s) = map fi $ filter (not . testBit s) [0 .. finiteBitSize s - 1]

regset_member :: (Num a, Integral a, FiniteBits b) => RegSet b -> a -> Bool
regset_member (RS s) r = testBit s (fi r)

regset_union :: FiniteBits b => RegSet b -> RegSet b -> RegSet b
regset_union (RS a) (RS b) = RS (a .|. b)

regset_intersect :: FiniteBits b => RegSet b -> RegSet b -> RegSet b
regset_intersect (RS a) (RS b) = RS (a .&. b)

regset_add :: (Num a, Integral a, FiniteBits b) => a -> RegSet b -> RegSet b
regset_add r (RS s) = RS (setBit s (fi r))

regset_remove :: (Num a, Integral a, FiniteBits b) => a -> RegSet b -> RegSet b
regset_remove r (RS s) = RS (clearBit s (fi r))
