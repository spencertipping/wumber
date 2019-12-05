{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module WumberTest.ConstraintSolver where

import Control.Monad (foldM)
import Control.Monad.RWS (evalRWS)
import Data.Either (rights)
import Data.Foldable (toList)
import Data.List (nub)
import Lens.Micro
import Linear.Metric (norm, distance)
import Linear.V2
import Linear.V3
import Test.QuickCheck
import Text.Printf

import qualified Data.IntMap as IM
import qualified Data.Vector as V

import Debug.Trace

import Wumber.Constraint
import Wumber.ConstraintSplit
import Wumber.ConstraintSimplify
import Wumber.ConstraintSolver
import Wumber.GeometricConstraints
import Wumber.Numeric
import Wumber.Symbolic


newtype UnitInterval a = UnitInterval a deriving Show
instance Arbitrary (UnitInterval R) where
  arbitrary = UnitInterval <$> choose (-1, 1)


instance Arbitrary (CVal ()) where arbitrary = val <$> arbitrary

instance Arbitrary (V2 R) where
  arbitrary = V2 <$> arbitrary <*> arbitrary

instance Arbitrary (V3 R) where
  arbitrary = V3 <$> arbitrary <*> arbitrary <*> arbitrary


instance (Eval R R [a] [b], DeterministicEval R R a b) =>
         DeterministicEval R R [a] [b]

instance DeterministicEval R R a b => Eval R R [a] [b] where
  eval t r = map (eval t r)


-- | A system is solvable iff it converges to error below the epsilon and hasn't
--   exhausted its iteration count.
--
--   GSL specifies tolerance in terms of /input/ distance, not output cost
--   function distance from the optimum -- so I'm verifying by assuming the
--   resulting cost will be at most √δ. There's no mathematical rigor to this
--   other than saying it's half as precise in log-terms.

solvable :: (Foldable f, DeterministicEval R R a R)
         => R -> Int -> Constrained () (f a) -> Property
solvable δ n m | isNaN cost || isInfinite cost = discard
               | isNaN v                       = discard
               | otherwise = counterexample (show (solution, cost, v, a))
                             $ v <= 1e-2
  where (a :: [R], solution) = solve δ n (toList <$> m)
        (_, subs) = ccompile m
        cost      = constraint_cost (concatMap _ss_constraints subs)
        v         = eval id (solution V.!) cost


solve_δ    = δ 1
iterations = 10000

t = solvable solve_δ iterations


prop_uni_linear :: R -> NonZero R -> CVal () -> CVal () -> Property
prop_uni_linear x (NonZero m) b y = t do
  v <- cvar x
  v * val m + b =-= y
  return [v]


prop_uni_quadratic :: NonNegative R -> NonNegative R -> Property
prop_uni_quadratic (NonNegative x) (NonNegative y) = t do
  v :: CVal () <- cvar x
  v*v =-= val y
  return [v]


prop_v2dist :: V2 R -> NonNegative R -> Property
prop_v2dist vec (NonNegative d) = t do
  v :: V2 (CVal ()) <- cvars vec
  norm v =-= val d
  return v


prop_v2joint_lt :: V2 R -> NonNegative R -> Property
prop_v2joint_lt vec (NonNegative d) = t do
  v :: V2 (CVal ()) <- cvars vec
  v^._x  <-= v^._y
  norm v =-= val d
  return v

prop_v2joint_eq :: V2 R -> NonNegative R -> Property
prop_v2joint_eq vec (NonNegative d) = t do
  v :: V2 (CVal ()) <- cvars vec
  v^._x  =-= v^._y
  norm v =-= val d
  return v


prop_v3dist :: V3 R -> NonNegative R -> Property
prop_v3dist vec (NonNegative d) = t do
  v :: V3 (CVal ()) <- cvars vec
  norm v =-= val d
  return v


{-
FIXME
...with new Sym implementation

prop_hexagon :: V2 R -> V2 R -> V2 R -> V2 R -> V2 R -> V2 R
             -> Positive R -> Property
prop_hexagon a b c d e f (Positive dist) =
  length (nub [a, b, c, d, e, f]) == 6 ==> t do
    av :: V2 (CVal ()) <- cvars a
    bv :: V2 (CVal ()) <- cvars b
    cv :: V2 (CVal ()) <- cvars c
    dv :: V2 (CVal ()) <- cvars d
    ev :: V2 (CVal ()) <- cvars e
    fv :: V2 (CVal ()) <- cvars f

    all_equal [val dist,
               distance av bv,
               distance bv cv,
               distance cv dv,
               distance dv ev,
               distance ev fv,
               distance fv av]

    {-
    all_equal [cos $ val (τ / 6),
               inner_angle_cos av bv cv,
               inner_angle_cos bv cv dv,
               inner_angle_cos cv dv ev,
               inner_angle_cos dv ev fv,
               inner_angle_cos ev fv av,
               inner_angle_cos fv av bv]
    -}

    aligned _x [av, ev]
    aligned _x [bv, dv]

    aligned _y   [av, bv]
    aligned _y [fv,     cv]
    aligned _y   [ev, dv]

    all_equal [distance av dv,
               distance bv ev,
               distance cv fv]

    return av
-}


prop_vec_varchains :: V2 R -> Property
prop_vec_varchains v = t $ vec_varchain 15 v

prop_varchains :: R -> Property
prop_varchains v = t $ varchain 30 v


vec_varchain :: Int -> V2 R -> Constrained () (V2 (CVal ()))
vec_varchain n v = do
  v0 <- cvars v
  vn <- foldM (\vn i -> do v' <- cvars v
                           v' =-= vn + 1
                           return v') v0 [1..n]
  vn =-= fmap val v
  return vn


varchain :: Int -> R -> Constrained () [CVal ()]
varchain 0 v = (: []) <$> cvar v
varchain n v = do vn <- cvar v
                  vc <- head <$> varchain (n - 1) v
                  vn =-= vc + 1
                  return [vn]

{-
To see how simplify handles large var chains, I tried this in the repl:

csimplify (rights (snd (evalRWS (vec_varchain 30 (V2 0 0)) () 0)))
csimplify (rights (snd (evalRWS (varchain 30 0) () 0)))

Currently, varchain works correctly while vec_varchain doesn't collapse
rewrites. varchain 60 is also much faster than vec_varchain 30.
-}


return []
runTests = $quickCheckAll
