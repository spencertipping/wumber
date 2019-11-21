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
type CVal = Sym R


-- | Constraints to be solved or minimized. Although both can be reduced to cost
--   functions, you should use 'CEqual' when possible because the solver can
--   often do some algebra up front to simplify equation systems before the
--   numerical step.
data Constraint = CEqual      !CVal !CVal
                | CMinimize   !CVal
                | CInitialize !Int !R
  deriving (Show, Eq, Generic, Binary)


-- | 'Constrained' is a monad that keeps track of 'Arg' IDs and collects
--   equivalences, quantities to minimize, and initial values.
type Constrained a = RWS () [Constraint] Int a


-- | Create a new constrained variable initialized to the given value.
var :: R -> Constrained CVal
var init = do id <- get
              modify (+ 1)
              tell [CInitialize id init]
              return $ Arg id

-- | A multidimensional variant of 'var'.
vars :: Traversable t => t R -> Constrained (t CVal)
vars = mapM var


-- | Constraint equivalence. The premise is that we can reduce each constraint
--   down to one or more scalar values that describe its out-of-whackness. The
--   solver attempts to set these values to zero.
class CEq a where
  infix 4 =-=; (=-=) :: a -> a -> Constrained ()
  infix 4 <-=; (<-=) :: a -> a -> Constrained ()
  infix 4 >-=; (>-=) :: a -> a -> Constrained ()
  (>-=) = flip (<-=)

instance {-# OVERLAPPABLE #-} CEq CVal where
  a =-= b = tell [CEqual a b]
  a <-= b = tell [CMinimize $ (b - a) `upper` N 0]

instance {-# OVERLAPPABLE #-} (Foldable f, CEq a) => CEq (f a) where
  a =-= b = sequence_ $ zipWith (=-=) (toList a) (toList b)
  a <-= b = sequence_ $ zipWith (<-=) (toList a) (toList b)
