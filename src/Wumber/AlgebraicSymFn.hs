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
--   > sym_apply = normalize_with sym_apply_cons
--
--   If you want to do things like collapsing common terms, then you should
--   provide that functionality by layering it onto 'sym_apply_cons'. You need
--   to do it there rather than after the fact because 'normalize_with' looks at
--   the output from whichever cons function you give it and in some cases will
--   choose the smallest of multiple possibilities. This behavior is governed by
--   'tree_size' via 'amb'.

module Wumber.AlgebraicSymFn where


import Data.List  (sortOn)
import Data.Maybe (fromMaybe)
import Lens.Micro ((&))

import qualified Data.Vector as V

import Wumber.Fingerprint
import Wumber.Functionable
import Wumber.SymExpr


-- | The class of functions that can specify algebraic structure. Idiomatically,
--   all of these are specified in terms of 'Maybe' because some algebraic
--   structures are parameterized.
class (Ord p, Eq f, Fingerprintable f, ProfileApply p f a) =>
      AlgebraicSymFn p f a | f -> p where
  commutativity  :: f -> Maybe (p -> p)
  associativity  :: f -> Maybe f
  distributivity :: f -> Maybe (f -> Bool)


-- | An algebraically-norming symbolic cons function wrapper.
normalize_with :: (Fingerprintable a, AlgebraicSymFn p f a)
               => (f -> [Sym p f a] -> Sym p f a)
               -> f -> [Sym p f a] -> Sym p f a
normalize_with cons f = cons f . cfn . afn . dfn
  where cfn = fromMaybe id (normalize_commutative    <$> commutativity f)
        afn = fromMaybe id (normalize_associative    <$> associativity f)
        dfn = fromMaybe id (normalize_distributive f <$> distributivity f)


-- | Chooses the simpler of two equivalent representations of the same logical
--   value.
amb :: [Sym p f a] -> [Sym p f a] -> [Sym p f a]
amb a b | sum (map tree_size a) < sum (map tree_size b) = a
        | otherwise                                     = b


-- | Considers applying distributivity, if the end result is more compact.
normalize_distributive :: f -> (f -> Bool) -> [Sym p f a] -> [Sym p f a]
normalize_distributive _ _ = id      -- TODO


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
