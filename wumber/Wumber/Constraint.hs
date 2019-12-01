{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | Numerically specified constraint equations. These are solved with a hybrid
--   strategy: first we partition the equation set into independent subsystems,
--   then we take each of those and isolate/substitute variables, and finally we
--   JIT a cost function with whichever variables remain and pass that to the
--   GSL BGFS solver.

module Wumber.Constraint where


import Control.Monad.RWS (RWS, get, tell, modify)
import Data.Binary       (Binary)
import Data.Either       (lefts, rights)
import Data.Foldable     (toList)
import GHC.Generics      (Generic(..))

import qualified Data.Set as S

import Wumber.ClosedComparable
import Wumber.Numeric
import Wumber.Symbolic


-- | A value being set to zero, symbolically or, failing that, numerically.
type CVal f = Sym f R


-- | 'Constrained' is a monad that keeps track of 'Var' IDs and collects
--   equivalences, quantities to minimize, and initial values. Generally you'd
--   use one such monad per independent constraint system within a project,
--   although it's fine to combine many into one: Wumber will figure out when
--   you have separable subsystems and solve them independently.
type Constrained f = RWS () [Either (VarID, R) (CVal f)] VarID


-- | Create a new constrained variable initialized to the specified value.
cvar :: FConstraints f R => R -> Constrained f (CVal f)
cvar init = do id <- get
               modify (+ 1)
               tell [Left (id, init)]
               return $ var id

-- | A multidimensional variant of 'cvar'.
cvars :: (FConstraints f R, Traversable t) => t R -> Constrained f (t (CVal f))
cvars = mapM cvar


-- | Sets two constrained quantities equal to each other.
set_equal :: FConstraints f R => CVal f -> CVal f -> CVal f
set_equal = (-)

-- | Sets one constrained quantity to be less than the other.
set_below :: FConstraints f R => CVal f -> CVal f -> CVal f
set_below a b = (b - a) `upper` 0


-- | Constraint equivalence. The premise is that we can reduce each constraint
--   down to one or more scalar values that describe its out-of-whackness. The
--   solver attempts to set these values to zero.
class CEq f a | a -> f where
  infix 4 =-=; (=-=) :: a -> a -> Constrained f ()
  infix 4 <-=; (<-=) :: a -> a -> Constrained f ()
  infix 4 >-=; (>-=) :: a -> a -> Constrained f ()
  (>-=) = flip (<-=)

instance {-# OVERLAPPABLE #-} FConstraints f R => CEq f (CVal f) where
  a =-= b = tell [Right $ b - a]
  a <-= b = tell [Right $ (b - a) `upper` 0]

instance {-# OVERLAPPABLE #-} (Foldable f, CEq t a) => CEq t (f a) where
  a =-= b = sequence_ $ zipWith (=-=) (toList a) (toList b)
  a <-= b = sequence_ $ zipWith (<-=) (toList a) (toList b)
