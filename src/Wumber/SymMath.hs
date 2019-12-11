{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
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
  SymMath',
  SymMathC,
  SymMathV(..),
  SymVars(..),
  v2, v3, v4,
  val,
  var,
  val_of,
  apply,
  apply',
  x_, y_, z_, t_,
  a_, b_, c_, d_,
) where


import Data.Binary   (Binary(..))
import Data.Foldable (foldl')
import Data.Either   (lefts, rights)
import Data.Function (on)
import Data.List     (find, groupBy, intercalate, partition, sort, sortOn)
import Data.Maybe    (catMaybes, fromJust, fromMaybe, isJust)
import GHC.Generics  (Generic)
import Linear.V2     (V2(..))
import Linear.V3     (V3(..))
import Linear.V4     (V4(..))
import Unsafe.Coerce (unsafeCoerce)

import Wumber.ClosedComparable
import Wumber.Fingerprint
import Wumber.Functionable
import Wumber.MathFn
import Wumber.SymExpr
import Wumber.SymMatch

import qualified Data.IntMap   as IM
import qualified Wumber.BitSet as BS


-- | Symbolic math expressions. Structurally identical to 'Sym', but typed to
--   implement Haskell numeric classes. @f@ should be specified to be
--   convertible from @MathFn@.
type SymMath f a = Math (SymMath' f a) SymMathPhantom
data SymMathPhantom

type SymMath' f = Sym (MathProfile f) f

-- | Class constraints that make most 'SymMath' things work.
type SymMathC f a = (MathFnC a,
                     SymVal a a,
                     SymbolicApply (MathProfile f) f a,
                     MathApply a (SymMath f a),
                     SymLift a (SymMath f a))

-- TODO: support real profiles
type MathProfile f = NoProfiles f


-- | A wrapper around 'sym_apply' that works for 'SymMath' quantities.
apply :: SymMathC f a => f -> [SymMath f a] -> SymMath f a
apply f = apply' f . map unMath

apply' :: SymMathC f a => f -> [SymMath' f a] -> SymMath f a
apply' f = Math . sym_apply f


instance Rewritable (SymMath' f a) =>
         Rewritable (SymMath f a) where
  rewrite (Math v) (mb, mv) = Math $ rewrite v (mb, IM.map unMath mv)


instance ProfileApply (MathProfile MathFn) MathFn where
  prof_val    = NP ()
  prof_var    = NP ()
  prof_fn _ _ = NP ()

instance SymLift a (SymMath' f a) => SymLift a (SymMath f a) where
  val     = Math . val
  var     = Math . var
  vars_in = vars_in . unMath

x_ = var 0 :: SymMath f a               -- ^ An alias for @var 0@
y_ = var 1 :: SymMath f a               -- ^ An alias for @var 1@
z_ = var 2 :: SymMath f a               -- ^ An alias for @var 2@
t_ = var 3 :: SymMath f a               -- ^ An alias for @var 3@

a_ = val (Math (As 0)) :: SymMath f (Match a)
b_ = val (Math (As 1)) :: SymMath f (Match a)
c_ = val (Math (As 2)) :: SymMath f (Match a)
d_ = val (Math (As 3)) :: SymMath f (Match a)


-- | A way to indicate that a symbolic quantity is acting as a vector function.
--   For example, @SymV V3 MathFn R@ should refer to vars 0, 1, and 2.
newtype SymMathV (v :: * -> *) f a = SymMathV { unSymMathV :: SymMath f a }
  deriving (Show, Eq, Generic, Binary)

v2 :: SymMathC f a => V2 (SymMath f a)
v2 = V2 (var 0) (var 1)

v3 :: SymMathC f a => V3 (SymMath f a)
v3 = V3 (var 0) (var 1) (var 2)

v4 :: SymMathC f a => V4 (SymMath f a)
v4 = V4 (var 0) (var 1) (var 2) (var 3)

-- | The class of vector spaces that can enumerate their dimensions in terms of
--   'Sym' variables and IDs. This is used by 'SymbolicDerivative'.
class SymVars (v :: * -> *) where
  vars :: SymMathC f a => v (SymMath f a)
  var_ids :: v VarID

instance SymVars V2 where vars = v2; var_ids = V2 0 1
instance SymVars V3 where vars = v3; var_ids = V3 0 1 2
instance SymVars V4 where vars = v4; var_ids = V4 0 1 2 3


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
  sym_apply = sym_apply_foldwith fn

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

instance (Eq a, MathFnC a, Fingerprintable a, SymVal a a) =>
         Functionable MathFn ([SymMath' MathFn a] -> SymMath' MathFn a) where
  fn = \case
    Negate -> \[x]    -> sym_apply Mul  [x, val (-1)]
    Recip  -> \[x]    -> sym_apply Pow  [x, val (-1)]
    Pow    -> \[x, y] -> sym_apply RPow [y, x]

    -- NOTE
    -- RPow is more useful to us because it stores the exponent in head
    -- position. That's consistent with the sort order for Add and Mul values,
    -- so we can reuse factor-handling logic.
    RPow -> \case
      [x, SymF RPow [y, z] _] -> sym_apply RPow [sym_apply Mul [x, y], z]
      [x, y] -> sym_apply_cons RPow [x, y]
      _      -> error "RPow is strictly binary"

    -- TODO
    -- This logic is a mess. We have several places where additive and
    -- multiplicative identities are provided. It wouldn't be wrong to use a
    -- typeclass to manage that stuff.

    -- TODO
    -- Move algebraic structure into SymAlgebra.

    Add -> \case []  -> val 0
                 [x] -> x
                 xs  -> cons_or (val 0) Add $ comm_assoc_fold Add Mul xs

    Mul -> \case []  -> val 1
                 [x] -> x
                 xs  -> try_distribute Mul Add $ comm_assoc_fold Mul RPow xs

    f -> sym_apply_cons f


-- | Commutative, associative normalization with constant folding.
comm_assoc_fold f g = term_collapse f g . commute_constants f . associative f

cons_or v f xs | null xs   = v
               | otherwise = sym_apply_cons f xs

associative f = concatMap \case SymF f' xs _ | f' == f -> xs
                                x                      -> [x]


-- | Distributive expansion for specific sub-operators. Operationally, we apply
--   this transformation:
--
--   > (f (g x y z) (g a b) c) -> (g (f x (g a b)) (f x c) (f y (g a b)) ...)

distribute f g xs = catMaybes $ flip map xs \case
  t@(SymF f' ys _) | f' == g -> Just $ sym_apply g [sym_apply f [x, y] |
                                                    x <- xs, y <- ys, x /= t]
  _ -> Nothing

try_distribute f g xs = ambs $ sym_apply_cons f xs : take 1 (distribute f g xs)


commute_constants :: (ValApply f a, SymVal a a)
                  => f -> [Sym p f a] -> [Sym p f a]
commute_constants f xs = SymC (val_apply f $ rights es) : lefts es
  where es = flip map xs \case x | Just v <- val_of x -> Right v
                               x                      -> Left x

term_collapse f g = sort
                    . catMaybes
                    . map (\case (_,   0)    -> Nothing
                                 ([SymC 0], _) -> Nothing
                                 ([x], 1)    -> Just x
                                 (xs,  1)    -> Just $ sym_apply g xs
                                 (xs,  v)    -> Just $ sym_apply g (val v : xs))
                    . sum_subterms
                    . expand_subterms g

  where expand_subterms f = sortOn fst . map
          \case SymF f' (SymC v : xs) _ | f' == f -> (xs,  v)
                SymF f' xs            _ | f' == f -> (xs,  1)
                x                                 -> ([x], 1)

        sum_subterms = map (\l@((x, _):_) -> (x, sum $ map snd l))
                       . groupBy ((==) `on` fst)


-- TODO
-- Clever stuff to make algebraic inversion work
math_fn_match :: (Eq a, Fingerprintable a)
              => MathFn -> MathFn
              -> [SymMath' MathFn (Match a)] -> [SymMath' MathFn a]
              -> Maybe [(VarID, SymMath' MathFn a)]
math_fn_match = fn_exact_match
