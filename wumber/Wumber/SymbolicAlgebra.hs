{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | Algebraic simplification/rewriting for 'Sym' quantities.
module Wumber.SymbolicAlgebra where


import Data.Binary  (Binary(..))
import GHC.Generics (Generic(..))

import Wumber.Symbolic


-- | Selective specializations of 'Sym' that reflect common algebraic pattern
--   matches.
data Alg a = Lit a
           | Alg a :+  Alg a
           | Alg a :-  Alg a
           | Alg a :*  Alg a
           | Alg a :/  Alg a
           | Alg a :%  Alg a
           | Alg a :** Alg a
           | Sin (Alg a)
           | Cos (Alg a)
           | AFn1 !SymFn1 (Alg a)
           | AFn2 !SymFn2 (Alg a) (Alg a)
  deriving (Show, Eq, Functor, Foldable, Traversable, Generic, Binary)

infixl 6 :+
infixl 6 :-
infixl 7 :*
infixl 7 :/
infixl 7 :%
infixl 8 :**


alg :: Sym a -> Alg (Sym a)
alg = Lit        -- TODO obviously


-- TODO
-- Use TemplateHaskell to consume pattern matches and use that to inform the
-- projection function about things like summed-term ordering. Basically, our
-- constructors have algebraic properties that Haskell isn't aware of, so we
-- need a normal form generator that does some conversion for us.
