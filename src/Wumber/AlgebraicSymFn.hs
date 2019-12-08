{-# LANGUAGE AllowAmbiguousTypes #-}
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

-- TODO
-- This implementation is a garbage fire. Most of this stuff should be done with
-- the natural ordering offered by profiles, not this way.

module Wumber.AlgebraicSymFn where


import Data.List  (groupBy, sortBy)
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
--
--   'associativity' can be implemented with the helper function 'fn_is'.
--   'commutativity' can be done with 'compare' for decent results, although
--   that isn't sufficient to reduce polynomial terms.

-- FIXME
-- This is a huge mess. We'll do a lot better with a pattern-matching approach;
-- let's keep the functions below but remove this typeclass.

class AlgebraicSymFn p f a where
  left_identity  :: f -> Maybe (Sym p f a -> Bool)
  right_identity :: f -> Maybe (Sym p f a -> Bool)
  inversion      :: f -> Maybe (Sym p f a -> Bool)
  associativity  :: f -> Maybe (Sym p f a -> Bool)
  commutativity  :: f -> Maybe (Sym p f a -> Sym p f a -> Ordering)
  fusion         :: f -> Maybe (Sym p f a -> Sym p f a -> Bool)
  fuse           :: f -> [Sym p f a] -> [Sym p f a]

  left_identity  _ = Nothing
  right_identity _ = Nothing
  inversion      _ = Nothing
  associativity  _ = Nothing
  commutativity  _ = Nothing
  fusion         _ = Nothing
  fuse           _ = id


-- | A function you can use with 'associative' to select operands that are
--   function applications and whose function is the one specified.
fn_is :: Eq f => f -> Sym p f a -> Bool
fn_is f (SymF g _ _) = f == g
fn_is _ _            = False


-- | A 'sym_apply_cons' modifier that 'normalize's its arguments first.
normalize_with :: AlgebraicSymFn p f a
               => (f -> [Sym p f a] -> Sym p f a)
               -> f -> [Sym p f a] -> Sym p f a
normalize_with cons f = cons f . normalize f


-- | Normalizes the operands to the given function by applying whichever
--   algebraic structures the function supports, always in this order:
--
--   1. Flatten sub-invocations if associative
--   2. Sort operands if commutative
--   3. Fuse operand groups if fuseable

normalize :: AlgebraicSymFn p f a => f -> [Sym p f a] -> [Sym p f a]
normalize f = ffn . cfn . afn . lifn . rifn
  where afn  = fromMaybe id $ normalize_associative <$> associativity f
        cfn  = fromMaybe id $ normalize_commutative <$> commutativity f
        ffn  = fromMaybe id $ (concatMap (fuse f) .) <$> groupBy <$> fusion f
        lifn = fromMaybe id $ normalize_lidentity <$> left_identity f
        rifn = fromMaybe id $ normalize_ridentity <$> right_identity f


normalize_lidentity :: (Sym p f a -> Bool) -> [Sym p f a] -> [Sym p f a]
normalize_lidentity f [] = []
normalize_lidentity f [x] = [x]
normalize_lidentity f (x:xs) | f x       =     normalize_lidentity f xs
                             | otherwise = x : normalize_lidentity f xs

normalize_ridentity :: (Sym p f a -> Bool) -> [Sym p f a] -> [Sym p f a]
normalize_ridentity f = reverse . normalize_lidentity f . reverse


-- | Normalizes operands to an associative operator by inlining any children
--   that have the same operator; that is, @(f (f x y) z)@ would become
--   @(f x y z)@.
normalize_associative :: (Sym p f a -> Bool) -> [Sym p f a] -> [Sym p f a]
normalize_associative f = concatMap \case o | f o -> V.toList (operands o)
                                          o       -> [o]


-- | Normalizes operands to a commutative operator by sorting them, not
--   necessarily by their natural ordering. The reason we ignore the natural
--   ordering is that the term profile doesn't always produce the most relevant
--   term groupings; for example, if we're commuting @+@ to normalize terms
--   within a polynomial, we'd want @2x@ and @3x@ to end up next to each other,
--   while @2x²@ would be grouped with other @x²@ terms.
normalize_commutative :: (Sym p f a -> Sym p f a -> Ordering)
                      -> [Sym p f a] -> [Sym p f a]
normalize_commutative = sortBy
