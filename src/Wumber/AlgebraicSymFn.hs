{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -funbox-strict-fields -Wincomplete-patterns #-}

-- | Definitions for functions with algebraic properties. This provides easy
--   ways to create normal forms for associative and/or commutative functions,
--   for example.
--
--   If your function implements 'AlgebraicSymFn', then you can create a
--   normalizing 'sym_apply' function like this:
--
--   > -- for constant folding (i.e. if you also implement ValApply):
--   > sym_apply = sym_apply_foldwith (normalize_with sym_apply_cons)
--   >
--   > -- without constant folding:
--   > sym_apply = normalize_wtih sym_apply_cons

-- TODO
-- Add support for distributivity. This is complicated by the fact that we'll
-- amb-reduce potentially many levels above where we do the expansion.

module Wumber.AlgebraicSymFn where


import Data.List  (groupBy, sortOn)
import Data.Maybe (fromMaybe)

import qualified Data.Vector as V

import Wumber.Fingerprint
import Wumber.Functionable
import Wumber.SymExpr


-- | The class of functions that can specify algebraic structure. Idiomatically,
--   all of these are specified in terms of 'Maybe' because any algebraic
--   property can be declined. Once you've implemented this class, you can get a
--   normalizing variant of 'sym_apply_cons' using 'normalize_with' and/or
--   'normalize'.
class (Ord p, Eq f, Fingerprintable f, ProfileApply p f a) =>
      AlgebraicSymFn p f a | f -> p where
  associativity :: f -> Maybe f
  commutativity :: f -> Maybe (p -> p)
  fusion        :: f -> Maybe (Sym p f a -> Sym p f a -> Bool,
                               [Sym p f a] -> [Sym p f a])


-- | A 'sym_apply_cons' modifier that 'normalize's its arguments first.
normalize_with :: (Fingerprintable a, AlgebraicSymFn p f a)
               => (f -> [Sym p f a] -> Sym p f a)
               -> f -> [Sym p f a] -> Sym p f a
normalize_with cons f = cons f . normalize f


-- | Normalizes the operands to the given function by applying whichever
--   algebraic structures the function supports, always in this order:
--
--   1. Flatten sub-invocations if associative
--   2. Sort operands if commutative
--   3. Fuse operand groups if fuseable

normalize :: (Fingerprintable a, AlgebraicSymFn p f a)
          => f -> [Sym p f a] -> [Sym p f a]
normalize f = ffn . cfn . afn
  where afn = fromMaybe id $ normalize_associative <$> associativity f
        cfn = fromMaybe id $ normalize_commutative <$> commutativity f
        ffn = fromMaybe id $ normalize_fusion      <$> fusion f


-- | Normalizes groups of adjacent operands, bounded by the specified
--   equivalence function. This is usually used in conjunction with
--   commutativity, but it doesn't have to be.
--
--   A typical use case for fusion is collapsing like terms within polynomials.

normalize_fusion ::
  (Eq p, ProfileApply p f a)
  => ((Sym p f a -> Sym p f a -> Bool), [Sym p f a] -> [Sym p f a])
  -> [Sym p f a] -> [Sym p f a]
normalize_fusion (f, g) = concatMap g . groupBy f


-- | Normalizes operands to an associative operator by inlining any children
--   that have the same operator; that is, @(f (f x y) z)@ would become
--   @(f x y z)@.
normalize_associative :: Eq f => f -> [Sym p f a] -> [Sym p f a]
normalize_associative f =
  concatMap \case SymF g xs _ | g == f -> V.toList xs
                  x                    -> [x]


-- | Normalizes operands to a commutative operator by sorting them, not
--   necessarily by their natural ordering. The reason we ignore the natural
--   ordering is that the term profile doesn't always produce the most relevant
--   term groupings; for example, if we're commuting @+@ to normalize terms
--   within a polynomial, we'd want @2x@ and @3x@ to end up next to each other,
--   while @2x²@ would be grouped with other @x²@ terms.
normalize_commutative :: (Fingerprintable a,
                          Fingerprintable f,
                          ProfileApply p f a,
                          Ord p)
                      => (p -> p) -> [Sym p f a] -> [Sym p f a]
normalize_commutative f = sortOn \s -> (f (profile s), fingerprint s)
