{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}


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
data Constraint = forall f. FConstraints f R => CEqual    !(CVal f) !(CVal f)
                | forall f. FConstraints f R => CMinimize !(CVal f)
                | CInitialize !VarID !R

deriving instance Show Constraint


-- | 'Constrained' is a monad that keeps track of 'Var' IDs and collects
--   equivalences, quantities to minimize, and initial values.
type Constrained = RWS () [Constraint] VarID


-- | Create a new constrained variable initialized to the given value.
cvar :: FConstraints f R => R -> Constrained (CVal f)
cvar init = do id <- get
               modify (+ 1)
               tell [CInitialize id init]
               return $ sym (Var id)

-- | A multidimensional variant of 'var'.
cvars :: (FConstraints f R, Traversable t) => t R -> Constrained (t (CVal f))
cvars = mapM cvar


-- | Constraint equivalence. The premise is that we can reduce each constraint
--   down to one or more scalar values that describe its out-of-whackness. The
--   solver attempts to set these values to zero.
class CEq a where
  infix 4 =-=; (=-=) :: a -> a -> Constrained ()
  infix 4 <-=; (<-=) :: a -> a -> Constrained ()
  infix 4 >-=; (>-=) :: a -> a -> Constrained ()
  (>-=) = flip (<-=)

instance {-# OVERLAPPABLE #-} FConstraints f R => CEq (CVal f) where
  a =-= b = tell [CEqual a b]
  a <-= b = tell [CMinimize $ (b - a) `upper` 0]

instance {-# OVERLAPPABLE #-} (Foldable f, CEq a) => CEq (f a) where
  a =-= b = sequence_ $ zipWith (=-=) (toList a) (toList b)
  a <-= b = sequence_ $ zipWith (<-=) (toList a) (toList b)
