{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | Numerically specified constraint equations. These are solved with a hybrid
--   strategy: first we partition the equation set into independent subsystems,
--   then we take each of those and isolate/substitute variables, and finally we
--   JIT a cost function with whichever variables remain and pass that to the
--   GSL BGFS solver.

module Wumber.Constraint where


import Control.Monad.RWS (RWS, get, tell, modify)
import Data.Binary       (Binary)
import Data.Foldable     (toList)
import GHC.Generics      (Generic(..))

import Wumber.ClosedComparable
import Wumber.Numeric
import Wumber.Symbolic


-- | A value being constrained in some way.
type CVal f = Sym f R


-- | Constraints to be solved or minimized. Although both can be reduced to cost
--   functions, you should use 'CEqual' when possible because the solver can
--   often do some algebra up front to simplify equation systems before the
--   numerical step.
data Constraint f = CEqual    !(CVal f) !(CVal f)
                  | CMinimize !(CVal f)
                  | CInitialize !VarID !R
  deriving (Show, Eq)


-- | 'Constrained' is a monad that keeps track of 'Var' IDs and collects
--   equivalences, quantities to minimize, and initial values.
type Constrained f = RWS () [Constraint f] VarID


-- | Create a new constrained variable initialized to the given value.
cvar :: FConstraints f R => R -> Constrained f (CVal f)
cvar init = do id <- get
               modify (+ 1)
               tell [CInitialize id init]
               return $ sym (Var id)

-- | A multidimensional variant of 'var'.
cvars :: (FConstraints f R, Traversable t) => t R -> Constrained f (t (CVal f))
cvars = mapM cvar


-- | Constraint equivalence. The premise is that we can reduce each constraint
--   down to one or more scalar values that describe its out-of-whackness. The
--   solver attempts to set these values to zero.
class CEq f a | a -> f where
  infix 4 =-=; (=-=) :: a -> a -> Constrained f ()
  infix 4 <-=; (<-=) :: a -> a -> Constrained f ()
  infix 4 >-=; (>-=) :: a -> a -> Constrained f ()
  (>-=) = flip (<-=)

instance {-# OVERLAPPABLE #-} FConstraints f R => CEq f (CVal f) where
  a =-= b = tell [CEqual a b]
  a <-= b = tell [CMinimize $ (b - a) `upper` 0]

instance {-# OVERLAPPABLE #-} (Foldable f, CEq t a) => CEq t (f a) where
  a =-= b = sequence_ $ zipWith (=-=) (toList a) (toList b)
  a <-= b = sequence_ $ zipWith (<-=) (toList a) (toList b)
