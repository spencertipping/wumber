{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -funbox-strict-fields -Wincomplete-patterns #-}

-- | Simple pattern matching over 'SymExpr' trees.

module Wumber.SymMatch where


import Data.Binary  (Binary)
import Data.Maybe   (fromJust, isJust)
import Data.Vector  (Vector)
import GHC.Generics (Generic)

import qualified Data.Vector as V

import Wumber.Fingerprint
import Wumber.Functionable
import Wumber.MathFn
import Wumber.SymExpr


-- | Matches a constant or extracts a value.
type Match a = Math (Match' a) MatchPhantom
data MatchPhantom

data Match' a = Is a | As VarID
  deriving (Ord, Eq, Functor, Generic, Binary)

instance Show a => Show (Match a) where
  show (Math (Is a)) = "=" ++ show a
  show (Math (As i)) = "[" ++ show i ++ "]"

instance SymVal (Match a) (Match a) where
  val_of v@(Math (Is x)) = Just v
  val_of _               = Nothing


-- | Matches a pattern against an expression and returns the subexpressions
--   bound by 'As' terminals.
match :: (Eq a, Eq f, Fingerprintable f, Fingerprintable a)
      => Sym p f (Match a) -> Sym p f a -> Maybe [Sym p f a]
match p e = V.toList <$> (V.generate (match_nvars p) undefined V.//) <$> go p e
  where go a b | fingerprint a == fingerprint b = Just []
        go (SymC (Math (As i))) x               = Just [(i, x)]
        go (SymF f xs _) (SymF g ys _)
          | f == g && V.length xs == V.length ys && all isJust ms =
            Just $ concatMap fromJust ms
          where ms = zipWith go (V.toList xs) (V.toList ys)
        go _ _ = Nothing


-- | Returns a profile-matching predicate function from a pattern. The profile
--   match will usually be much faster than tree-scanning.
pattern_to_profile :: ProfileApply p f => Sym p f (Match a) -> p -> Bool
pattern_to_profile _ = error "TODO"


-- | Takes a 'SymExpr' of variables and turns it into a 'SymExpr' of matches
--   that refers to no variables.
--
--   TODO: specify which variables we want to keep?

pattern_of :: (Fingerprintable f, Fingerprintable a, ProfileApply p f)
           => (f -> [Sym p f (Match a)] -> Sym p f (Match a))
           -> Sym p f a -> Sym p f (Match a)
pattern_of _    (SymC x)      = SymC (Math (Is x))
pattern_of _    (SymV i)      = SymC (Math (As i))
pattern_of cons (SymF f xs _) = cons f $ map (pattern_of cons) $ V.toList xs


match_nvars :: Sym p f (Match a) -> VarID
match_nvars = (1 +) . flip foldr (-1) \case Math (As v) -> max v
                                            Math (Is a) -> id


-- | This instance deserves some explanation.
--
--   'match', like a lot of our algebraic stuff, uses 'fingerprint' to bypass
--   tree comparisons whenever possible. Since @Is x@ matches exactly the
--   constant @x@, we can shortcut by saying that @Is x@ is the same as @x@ from
--   a fingerprint perspective.
--
--   'As' always requires a visit because we need to bind the quantity. This
--   means that not only is it unequal to, say, a variable, it's in fact unequal
--   to /everything/ we might try to match against. A simple way to guarantee
--   this is to generate some hidden randomness that we don't expose via any
--   API, then use that randomness as hash input entropy. Nobody will match our
--   fingerprints unless they have the same entropy.

instance Fingerprintable a => Fingerprintable (Match a) where
  fingerprint (Math (Is a)) = fingerprint a
  fingerprint (Math (As i)) = binary_fingerprint (match_secret, i)
    where match_secret = fingerprint "532d0517-8c14-4e23-b3af-1f8d1f1c1890"
    -- NOTE: don't use this value anywhere else in code. If you do, pattern
    -- matching may produce inaccurate results.


instance MathFnC a => MathApply a (Match a) where
  fn0 = Math . Is
  fn1 f (Math (Is x)) | Just f' <- fn f = Math (Is (f' x))
  fn1 f _ =
    error $ "can't transform As matches with math operators (" ++ show f ++ ")"

  fn2 f (Math (Is x)) (Math (Is y)) | Just f' <- fn f = Math (Is (f' x y))
  fn2 f _ _ =
    error $ "can't transform As matches with math operators (" ++ show f ++ ")"

  fn3 f (Math (Is x)) (Math (Is y)) (Math (Is z))
    | Just f' <- fn f = Math (Is (f' x y z))

  fn3 f _ _ _ =
    error $ "can't transform As matches with math operators (" ++ show f ++ ")"
