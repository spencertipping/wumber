{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -funbox-strict-fields -Wincomplete-patterns #-}

-- | A @newtype@ around 'Sym' that provides Haskell 'Num', 'Floating', and other
--   such support. You can construct 'SymMath' quantities using 'val' and 'var',
--   e.g.
--
--   > x :: SymMath MathFn Double
--   > x = var 0 ** 2 + 1
--
--   Not all math operators are supported. Unsupported operators will
--   immediately throw a runtime error when used, regardless of the const-ness
--   of your symbolic expression. To extract a constant as a non-symbolic
--   quantity, use 'val_of'.

module Wumber.SymMath (
  SymMath(..),
  val,
  var,
  val_of,
  x_, y_, z_, t_,
  a_, b_, c_, d_,
) where


import Data.Binary   (Binary)
import Data.Foldable (foldl')
import Data.Either   (lefts, rights)
import Data.Function (on)
import Data.List     (groupBy, intercalate, sort, sortOn)
import Data.Maybe    (fromJust, isJust)
import GHC.Generics  (Generic)
import Lens.Micro    ((&))
import Unsafe.Coerce (unsafeCoerce)

import Wumber.ClosedComparable
import Wumber.Fingerprint
import Wumber.Functionable
import Wumber.MathFn
import Wumber.SymExpr
import Wumber.SymMatch


-- | Symbolic math expressions. Structurally identical to 'Sym', but typed to
--   implement Haskell numeric classes. @f@ should be specified to be
--   convertible from @MathFn@.
type SymMath f a = Math (SymMath' f a) SymMathPhantom
data SymMathPhantom

type SymMath' f = Sym (MathProfile f) f

-- TODO: support real profiles
type MathProfile f = NoProfiles f

instance ProfileApply (MathProfile MathFn) MathFn where
  prof_val    = NP ()
  prof_var    = NP ()
  prof_fn _ _ = NP ()

instance SymLift a (SymMath' f a) => SymLift a (SymMath f a) where
  val = Math . val
  var = Math . var

x_ = var 0 :: SymMath f a               -- ^ An alias for @var 0@
y_ = var 1 :: SymMath f a               -- ^ An alias for @var 1@
z_ = var 2 :: SymMath f a               -- ^ An alias for @var 2@
t_ = var 3 :: SymMath f a               -- ^ An alias for @var 3@

a_ = val (Math (As 0)) :: SymMath f (Match a)
b_ = val (Math (As 1)) :: SymMath f (Match a)
c_ = val (Math (As 2)) :: SymMath f (Match a)
d_ = val (Math (As 3)) :: SymMath f (Match a)


instance SymVal a b => SymVal (SymMath f a) b where val_of = val_of . unMath

instance SymVal Double Double where val_of = Just
instance SymVal Float  Float  where val_of = Just

-- NOTE
-- This is squirrelly. I'm doing this so we can get floating-point mod, but this
-- implementation is arguably wrong (see discussion on the 'qr' function in
-- MathFn).
instance Integral Double where toInteger = truncate; quotRem = qr
instance Integral Float  where toInteger = truncate; quotRem = qr


instance FnMatch MathFn (Sym (MathProfile MathFn) MathFn) where
  fn_match = math_fn_match

instance (Show a, FnShow f) => Show (SymMath f a) where
  show (Math (SymC x)) = show x
  show (Math (SymV i)) | i == 0 = "x_"
                       | i == 1 = "y_"
                       | i == 2 = "z_"
                       | i == 3 = "t_"
                       | otherwise = "v" ++ show i

  show (Math (SymF f xs _)) =
    fshow f $ map (show . (Math :: Sym (MathProfile f) f a -> SymMath f a)) xs

instance (MathFnC a, SymVal a a, Fingerprintable a,
          ProfileApply (MathProfile MathFn) MathFn) =>
         SymbolicApply (MathProfile MathFn) MathFn a where
  sym_apply = sym_apply_foldwith math_sym_apply

instance MathFnC a => ValApply MathFn a where
  val_apply Add [] = 0
  val_apply Mul [] = 1

  val_apply mf [x, y, z] | Just f <- fn mf = f x y z
  val_apply mf [x]       | Just f <- fn mf = f x
  val_apply mf (x:xs)    | Just f <- fn mf = foldl' f x xs
  val_apply mf xs = error $ "can't apply " ++ show mf
                    ++ " to list of arity " ++ show (length xs)

instance (MathFnC a, SymVal a a, Fingerprintable a) =>
         MathApply a (SymMath MathFn a) where
  fn0                              = val
  fn1 f (Math x)                   = Math $ sym_apply f [x]
  fn2 f (Math x) (Math y)          = Math $ sym_apply f [x, y]
  fn3 f (Math x) (Math y) (Math z) = Math $ sym_apply f [x, y, z]


-- | A 'sym_apply' function that reduces certain cases to normal forms so we can
--   combine and/or cancel terms. The main focus is polynomial stuff, which
--   means we have two levels of reduction:
--
--   1. Additive cancellation of multiplicative terms
--   2. Multiplicative cancellation of exponential terms
--
--   Normalizing terms will sometimes complicate things: we don't always end up
--   with simpler expressions by distributing multiplication or exponentiation.
--   Because of this, we use 'amb' to conditionally reject more complex results.
--
--   Each layer of normalization relies on the ones below it; that is, we know
--   up front that each operand is normalized by the time we're 'sym_apply'ing
--   something on top of it.

math_sym_apply :: (Eq a, MathFnC a, Fingerprintable a, SymVal a a)
               => MathFn
               -> [Sym (MathProfile MathFn) MathFn a]
               -> Sym (MathProfile MathFn) MathFn a
math_sym_apply f xs = case (f, xs) of
  (Negate, [x]) -> sym_apply Mul [x, val (-1)]
  (Recip,  [x]) -> sym_apply Pow [x, val (-1)]
  (Pow, [x, y]) -> sym_apply RPow [y, x]

  (RPow, [SymC x, SymF RPow [SymC y, z] _]) -> sym_apply RPow [SymC (x*y), z]
  (RPow, [x@(SymC _), y@(SymF Mul ys _)])   -> try_distribute RPow Mul x ys y

  (Add, []) -> val 0
  (Mul, []) -> val 1
  (Add, xs) -> comm_assoc_fold Add Mul xs
  (Mul, xs) -> comm_assoc_fold Mul RPow xs
  _         -> sym_apply_cons f xs


-- | Commutative, associative normalization with constant folding.
comm_assoc_fold f g = term_collapse f g . commute_constants f . associative f

associative f = concatMap \case SymF f' xs _ | f' == f -> xs
                                x                      -> [x]

try_distribute f g x ys y = amb a b
  where a = sym_apply_cons f [x, y]
        b = sym_apply g $ map (sym_apply f . (x :) . (: [])) ys

commute_constants :: (ValApply f a, SymVal a a)
                  => f -> [Sym p f a] -> [Sym p f a]
commute_constants f xs = SymC (val_apply f $ rights es) : lefts es
  where es = flip map xs \case x | Just v <- val_of x -> Right v
                               x                      -> Left x

expand_subterms f = sortOn fst . map
  \case SymF f' (SymC v : xs) _ | f' == f -> (xs,  v)
        x                                 -> ([x], 1)

sum_subterms = filter ((/= 0) . snd)
               . map (\l@((x, _):_) -> (x, sum $ map snd l))
               . groupBy ((==) `on` fst)

term_collapse f g xs | null ns   = val 0
                     | otherwise = sym_apply_cons f ns
  where ns = xs
             & expand_subterms g
             & sum_subterms
             & map (\case ([x], 1) -> x
                          (xs, v)  -> sym_apply g (val v : xs))
             & sort


-- TODO
-- Clever stuff to make algebraic inversion work
math_fn_match :: (Eq a, Fingerprintable a)
              => MathFn -> MathFn
              -> [SymMath' MathFn (Match a)] -> [SymMath' MathFn a]
              -> Maybe [(VarID, SymMath' MathFn a)]
math_fn_match = fn_exact_match
