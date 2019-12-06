{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{-# OPTIONS_GHC -funbox-strict-fields -Wincomplete-patterns #-}

module Wumber.SymMatch where


import Data.Binary  (Binary)
import Data.Maybe   (fromJust, isJust)
import Data.Vector  (Vector)
import GHC.Generics (Generic)

import qualified Data.Vector as V

import Wumber.Fingerprint
import Wumber.SymExpr


-- | Matches a constant or extracts a value.
data Match a = Is a | As VarID
  deriving (Show, Ord, Eq, Functor, Generic, Binary)


pattern_of :: (Fingerprintable f, Fingerprintable a, ProfileApply p f)
           => (f -> [Sym p f (Match a)] -> Sym p f (Match a))
           -> Sym p f a -> Sym p f (Match a)
pattern_of _    (SymC x)      = SymC (Is x)
pattern_of _    (SymV i)      = SymC (As i)
pattern_of cons (SymF f xs _) = cons f $ map (pattern_of cons) $ V.toList xs


match_nvars :: Sym p f (Match a) -> VarID
match_nvars = (1 +) . flip foldr (-1) \case As v -> max v
                                            Is a -> id


match :: (Eq a, Eq f)
      => Sym p f (Match a) -> Sym p f a -> Maybe (Vector (Sym p f a))
match p e = (V.generate (match_nvars p) undefined V.//) <$> go p e
  where go (SymC (As i)) x                 = Just [(i, x)]
        go (SymC (Is x)) (SymC y) | x == y = Just []
        go (SymV i) (SymV j)      | i == j = Just []
        go (SymF f xs _) (SymF g ys _)
          | f == g && V.length xs == V.length ys && all isJust ms =
            Just $ concatMap fromJust ms
          where ms = zipWith go (V.toList xs) (V.toList ys)
        go _ _ = Nothing


instance Fingerprintable a => Fingerprintable (Match a) where
  fingerprint (Is a) = fingerprint a
  fingerprint _      = Fingerprint 0 0

instance Num a => Num (Match a) where
  fromInteger = Is . fromInteger
  (+)    = error "(+) unimplemented for Match"
  (*)    = error "(*) unimplemented for Match"
  negate = error "negate unimplemented for Match"
  abs    = error "abs unimplemented for Match"
  signum = error "signum unimplemented for Match"
