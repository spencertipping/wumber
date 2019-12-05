{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | Bit sets optimized for very fast access of the first /n/ elements (where
--   /n/ is suitably small, e.g. less than 64). These bitsets use unboxed
--   storage whenever possible.
--
--   These bitsets are optimized to avoid cache misses more than they are for
--   strict CPU or GC efficiency.
--
--   This module is intended to be imported @qualified@.

module Wumber.BitSet where


import Data.Binary   (Binary)
import Data.Bits
import Data.Foldable (foldl')
import Data.Word     (Word64)
import GHC.Generics  (Generic)


-- | A BitSet the first 64 of whose bits are stored in an immediate unboxed
--   word. This should result in fewer cache misses to access those elements,
--   and we expect those to represent the majority of our accesses.
data BitSet = BS !Word64 !Integer deriving (Eq, Ord, Generic, Binary)

instance Show BitSet where show bs = "BitSet " ++ show (toList bs)


empty :: BitSet
empty = BS 0 0


is_empty :: BitSet -> Bool
is_empty (BS 0 0) = True
is_empty _        = False


singleton :: Int -> BitSet
singleton x | x < 64 = BS (1 `shiftL` x) 0
singleton x          = BS 0 (1 `shiftL` (x - 64))


member :: Int -> BitSet -> Bool
member b (BS h _) | b < 64 = testBit h b
member b (BS _ t)          = testBit t (b - 64)


union :: BitSet -> BitSet -> BitSet
union (BS h1 t1) (BS h2 t2) = BS (h1 .|. h2) (t1 .|. t2)

unions :: Foldable f => f BitSet -> BitSet
unions = foldl' union empty


intersect :: BitSet -> BitSet -> BitSet
intersect (BS h1 t1) (BS h2 t2) = BS (h1 .&. h2) (t1 .&. t2)

intersects :: Foldable f => f BitSet -> BitSet
intersects = foldl' intersect empty


highestBit :: BitSet -> Int
highestBit (BS h t) | t == 0    = bisect h 0 64
                    | otherwise = find_tbit 1 + 64

  where find_tbit i | shiftL 1 i > t = bisect t i (shiftR i 1)
                    | otherwise      = find_tbit (shiftL i 1)

        bisect b l u | u <= l + 1     = l
                     | shiftL 1 m > b = bisect b l m
                     | otherwise      = bisect b m u
          where m = shiftR (l+u) 1


-- TODO: optimize by skipping bits strategically
toList :: BitSet -> [Int]
toList b = filter (flip member b) [0 .. highestBit b]


fromList :: [Int] -> BitSet
fromList = foldl' (\s i -> s `union` singleton i) empty
