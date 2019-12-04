{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE CPP #-}

module WumberTest.Symbolic where

import Test.QuickCheck
import Test.QuickCheck.Function

import Wumber.Numeric
import Wumber.Symbolic
import Wumber.SymbolicDerivative


data Op = One SymFn1
        | Two SymFn2 (Sym () R)
  deriving (Eq, Show)


op :: Sym () R -> Op -> Sym () R
op s (One f)   = fn f s
op s (Two f x) = fn f s x


instance Arbitrary Op where
  arbitrary = frequency [(4, One <$> arbitrary),
                         (1, Two <$> arbitrary <*> arbitrary)]


instance Arbitrary (Sym () R) where
#if 1
  arbitrary = frequency [(10, val <$> arbitrary),
                         (5, var <$> choose (1, 4)),
                         (1, foldl op <$> arbitrary <*> (arbitrary :: Gen [Op]))]
#else
  arbitrary = val <$> arbitrary
#endif


instance Arbitrary SymFn1 where arbitrary = arbitraryBoundedEnum
instance Arbitrary SymFn2 where arbitrary = arbitraryBoundedEnum


-- Sym shouldn't care about variable IDs.


testfn = (var 0 + var 1) ** 2 :: Sym () R
testfn_dx = testfn //= [(0, var 0 + 1)]
testfn_dy = testfn //= [(1, var 1 + 1)]

prop_simple :: R -> R -> Property
prop_simple x y = (testfn_dx //= [(0, val x), (1, val y)]) === (testfn_dy //= [(0, val y), (1, val x)])


prop_it_works :: [Op] -> R -> VarID -> (R, R, R, R) -> Property
prop_it_works ops v i (a, b, c, d) =
  counterexample (show (f (var 0), lhs, rhs)) $ lhs `approx` rhs
  where lhs = f (var 0) //= [(0, val v)]
        rhs = f (val v)

        f x = foldl op x ops //= zip [1..] (map val [a, b, c, d])
        approx a b = isNaN a || isNaN b || isInfinite a || isInfinite b
                     || abs (a - b) < 1e-8


return []
runTests = $quickCheckAll
