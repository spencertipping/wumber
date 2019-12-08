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


import Data.Binary   (Binary)
import Data.Foldable (foldr')
import Data.Function (on)
import Data.List     (groupBy, sortOn)
import Data.Maybe    (fromJust, isJust)
import Data.Vector   (Vector)
import GHC.Generics  (Generic)

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


-- | The class of functions that can provide overloaded pattern matching. This
--   is useful when arguments don't need to be matched exactly; e.g. when a
--   function is commutative, associative, or has arguments that are invertible.
class FnMatch f t where
  fn_match :: (Eq a, Fingerprintable a)
           => f -> f -> [t (Match a)] -> [t a] -> Maybe [(VarID, t a)]


-- | A 'fn_match' implementation for functions with no algebraic structure.
fn_exact_match :: (Eq a, Eq f, Fingerprintable f, Fingerprintable a,
                   FnMatch f (Sym p f))
               => f -> f -> [Sym p f (Match a)] -> [Sym p f a]
               -> Maybe [(VarID, Sym p f a)]
fn_exact_match f g ps es
  | f == g && samesize && all isJust vs = Just $ concatMap fromJust vs
  | otherwise                           = Nothing
  where vs       = zipWith match' ps es
        samesize = length ps == length es


-- | Matches a pattern against an expression and returns the subexpressions
--   bound by 'As' terminals.

-- TODO: faster collision rejection?
match :: (Eq a, Eq f, Fingerprintable f, Fingerprintable a, FnMatch f (Sym p f))
      => Sym p f (Match a) -> Sym p f a -> Maybe [Sym p f a]
match p e = match' p e >>= verify >>= return . unify
  where unify = V.toList . (V.replicate (match_nvars p) undefined V.//)
        verify xs | same      = Just xs
                  | otherwise = Nothing
          where same = all (\((_, x):xs) -> all ((== x) . snd) xs)
                       $ groupBy ((==) `on` fst)
                       $ sortOn fst xs


-- | The worker function for 'match'. This function returns @(VarID, Sym)@ pairs
--   instead of a dense list.
match' a b | fingerprint a == fingerprint b = Just []
match' (SymC (Math (As i))) x               = Just [(i, x)]
match' (SymF f xs _) (SymF g ys _) = fn_match f g xs ys
match' _ _ = Nothing


-- | Returns a profile-matching predicate function from a pattern. The profile
--   match will usually be much faster than tree-scanning.

-- TODO: what's the strategy here unless we know it's Bits and we know the
-- layout? Profile doesn't give us enough information to know how to select
-- specific pieces ... yet.
pattern_to_profile :: ProfileApply p f => Sym p f (Match a) -> p -> Bool
pattern_to_profile = error "TODO"


match_nvars :: Sym p f (Match a) -> VarID
match_nvars = (1 +) . flip foldr' (-1) \case Math (As v) -> max v
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
