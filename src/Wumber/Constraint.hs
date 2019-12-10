{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | Numerically specified constraint equations. These are solved with a hybrid
--   strategy: first we partition the equation set into independent subsystems,
--   then we take each of those and isolate/substitute variables, and finally we
--   JIT a cost function with whichever variables remain and pass that to the
--   GSL minimizer.
--
--   Constraint systems are used both for parametric modeling and for some
--   finite element simulations. The main priority is to be able to scale out to
--   hundreds or thousands of variables.

module Wumber.Constraint where


import Control.Monad.RWS (RWS, evalRWS, get, modify', tell)
import Data.Binary       (Binary)
import Data.Either       (lefts, rights)
import Data.Foldable     (toList)
import GHC.Generics      (Generic(..))

import qualified Data.Binary as B
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS

import Wumber.ClosedComparable
import Wumber.EquationSystem
import Wumber.Fingerprint
import Wumber.Numeric
import Wumber.SymAlgebra
import Wumber.SymExpr
import Wumber.SymMath


-- | A value being set to zero symbolically or, failing that, numerically.
type CVal f = SymMath f R


-- | 'Constrained' is a monad that keeps track of 'Var' IDs and collects
--   equivalences, quantities to minimize, and initial values. Generally you'd
--   use one such monad per independent constraint system within a project,
--   although it's fine to combine many into one: Wumber will figure out when
--   you have separable subsystems and solve them independently.
--
--   'Constrained' monad instances are both serializable and 'Fingerprintable'.
--   This means you can build a @Computed (Constrained f a) _@ and Wumber will
--   memoize the constraint solution.

-- TODO
-- Rewrite constraint variables as we discover equivalence. This means we need
-- to replace [] with something that does more active management.
--
-- I think we can replace ConstraintSimplify with an incremental approach,
-- probably just Monoid.

type Constrained f = RWS () [Either (VarID, R) (CVal f)] VarID

instance (Binary a, Binary f) => Binary (Constrained f a) where
  put m = B.put $ evalRWS m () 0
  get = do (a, cs) <- B.get
           return $ tell cs >> return a

instance (Binary a, Binary f) => Fingerprintable (Constrained f a) where
  fingerprint = binary_fingerprint


-- | Create a new constrained variable initialized to the specified value.
cvar :: R -> Constrained f (CVal f)
cvar init = do id <- get
               modify' (+ 1)
               tell [Left (id, init)]
               return $ var id

-- | A multidimensional variant of 'cvar'.
cvars :: (SymMathC f R, Traversable t) => t R -> Constrained f (t (CVal f))
cvars = mapM cvar


-- | Sets two constrained quantities equal to each other. When the two
--   quantities yield isolatable terms, adds entries to the substitution map.
set_equal :: SymMathC f R => CVal f -> CVal f -> Constrained f (CVal f)
set_equal a b = do let v = a - b
                   tell [Right v]
                   return v

-- | Sets one constrained quantity to be bounded above by another.
set_below :: SymMathC f R => CVal f -> CVal f -> Constrained f (CVal f)
set_below a b = do let v = (b - a) `upper` 0
                   tell [Right v]
                   return v


-- | Constraint equivalence. The premise is that we can reduce each constraint
--   down to one or more scalar values that describe its out-of-whackness. The
--   solver attempts to set these values to zero.
class SymMathC f R => CEq f a | a -> f where
  infix 4 =-=; (=-=) :: a -> a -> Constrained f ()
  infix 4 <-=; (<-=) :: a -> a -> Constrained f ()
  infix 4 >-=; (>-=) :: a -> a -> Constrained f ()
  (>-=) = flip (<-=)

instance {-# OVERLAPPABLE #-} SymMathC f R => CEq f (CVal f) where
  (=-=) = set_equal >> return ()
  (<-=) = set_below >> return ()

instance {-# OVERLAPPABLE #-} (Foldable f, CEq t a) => CEq t (f a) where
  a =-= b = sequence_ $ zipWith (=-=) (toList a) (toList b)
  a <-= b = sequence_ $ zipWith (<-=) (toList a) (toList b)
