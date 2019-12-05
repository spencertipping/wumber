{-# LANGUAGE TemplateHaskell #-}

module WumberTest.BitSet where

import Test.QuickCheck

import qualified Data.IntSet   as IS
import qualified Wumber.BitSet as BS


prop_list :: [NonNegative Int] -> Property
prop_list xs =
  IS.toList (IS.fromList bs) === BS.toList (BS.fromList bs)
  where bs = map getNonNegative xs


prop_member :: [NonNegative Int] -> NonNegative Int -> Property
prop_member xs (NonNegative i) =
  IS.member i (IS.fromList bs) === BS.member i (BS.fromList bs)
  where bs = map getNonNegative xs


return []
runTests = $quickCheckAll
