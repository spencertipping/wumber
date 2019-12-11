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


import Control.Monad.RWS (RWS, runRWS, gets, modify', tell)
import Data.Binary       (Binary)
import Data.Either       (lefts, rights)
import Data.Foldable     (toList)
import GHC.Generics      (Generic(..))
import Lens.Micro        (_1, _2, (%~))

import qualified Data.Binary as B
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS

import Wumber.ClosedComparable
import Wumber.EquationSolve
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

type Constrained f = RWS () [(VarID, R)] (VarID, EquationSystem f R)

instance (Binary a, Binary f) => Binary (Constrained f a) where
  put m = B.put $ runRWS m () (0, init_es)
  get = do (a, s, i) <- B.get
           return $ tell i >> modify' (const s) >> return a

instance (Binary a, Binary f) => Fingerprintable (Constrained f a) where
  fingerprint = binary_fingerprint


-- | Create a new constrained variable initialized to the specified value.
cvar :: R -> Constrained f (CVal f)
cvar init = do id <- gets fst
               modify' $ _1 %~ (+ 1)
               tell [(id, init)]
               return $ var id

-- | A multidimensional variant of 'cvar'.
cvars :: (SymMathC f R, Traversable t) => t R -> Constrained f (t (CVal f))
cvars = mapM cvar


-- | Evaluates a set of constraints and returns the solution, rewritten with the
--   solved values.
csolve :: (Eq a, SymLift R a, SymMathC f R, Rewritable a, Functor g)
       => Constrained f (g a) -> (g a)
csolve m = (// [(v, val x) | (v, x) <- IM.toList solution]) <$> r
  where (r, (_, es), ivs) = runRWS m () (0, init_es)
        solution          = solve default_settings (IM.fromList ivs) es


-- | Sets two constrained quantities equal to each other.
set_equal :: (Invertible (SymMath f R), SymMathC f R)
          => CVal f -> CVal f -> Constrained f (CVal f)
set_equal a b = do let v = a - b
                   modify' $ _2 %~ constrain v
                   return v

-- | Sets one constrained quantity to be bounded above by another.
set_below :: (Invertible (SymMath f R), SymMathC f R)
          => CVal f -> CVal f -> Constrained f (CVal f)
set_below a b = do let v = (b - a) `upper` 0
                   modify' $ _2 %~ constrain v
                   return v


-- | Constraint equivalence. The premise is that we can reduce each constraint
--   down to one or more scalar values that describe its out-of-whackness. The
--   solver attempts to set these values to zero.
class SymMathC f R => CEq f a | a -> f where
  infix 4 =-=; (=-=) :: a -> a -> Constrained f ()
  infix 4 <-=; (<-=) :: a -> a -> Constrained f ()
  infix 4 >-=; (>-=) :: a -> a -> Constrained f ()
  (>-=) = flip (<-=)

instance {-# OVERLAPPABLE #-} (Invertible (SymMath f R), SymMathC f R) =>
                              CEq f (CVal f) where
  a =-= b = set_equal a b >> return ()
  a <-= b = set_below a b >> return ()

instance {-# OVERLAPPABLE #-} (Foldable f, CEq t a) => CEq t (f a) where
  a =-= b = sequence_ $ zipWith (=-=) (toList a) (toList b)
  a <-= b = sequence_ $ zipWith (<-=) (toList a) (toList b)
