{-# LANGUAGE MonoLocalBinds #-}
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
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | Numerically specified constraint equations. These are solved with a hybrid
--   strategy: first we partition the equation set into independent subsystems,
--   then we take each of those and isolate/substitute variables, and finally we
--   JIT a cost function with whichever variables remain and pass that to the
--   GSL BGFS solver.

-- TODO
-- WTF ... why do I even have CEqual etc? If the goal is to minimize CVals, then
-- we can do it by setting some expression to zero; that's an implied equation
-- and good enough for 'isolate'.
--
-- Let's rework this and get rid of the 'Constraint' type.

-- TODO
-- Do we need initial values? Can we set everything to zero instead? (I guess
-- the problem with that is that if you do need an initial value, you'll
-- compromise your solution to specify it.)

module Wumber.Constraint where


import Control.Monad.RWS (RWS, get, tell, modify)
import Data.Binary       (Binary)
import Data.Foldable     (toList)
import GHC.Generics      (Generic(..))

import qualified Data.Set as S

import Wumber.ClosedComparable
import Wumber.Numeric
import Wumber.Symbolic


-- | A value being constrained by one of the 'Constraint' variants.
type CVal f = Sym f R

type CConstraints f = (FConstraints f R, Functionable f ([CVal f] -> CVal f))


-- | Constraints to be solved or minimized. Although both can be reduced to cost
--   functions, you should use 'CEqual' when possible because the solver can
--   often do some algebra up front to simplify equation systems before the
--   numerical step.
data Constraint f = CEqual    !(CVal f) !(CVal f)
                  | CMinimize !(CVal f)
                  | CInitialize !VarID !R
  deriving (Show, Eq)

instance CConstraints f => Eval R (CVal f) (Constraint f) (Constraint f) where
  eval t f (CEqual a b)      = CEqual (eval t f a) (eval t f b)
  eval t f (CMinimize a)     = CMinimize (eval t f a)
  eval _ _ (CInitialize _ _) = error "can't evaluate CInitialize"


-- | 'Constrained' is a monad that keeps track of 'Var' IDs and collects
--   equivalences, quantities to minimize, and initial values.
type Constrained f = RWS () [Constraint f] VarID


-- | The full set of variables referred to by a constraint.
constraint_deps :: FConstraints f R => Constraint f -> S.Set VarID
constraint_deps (CEqual a b)      = vars_in a `S.union` vars_in b
constraint_deps (CMinimize v)     = vars_in v
constraint_deps (CInitialize i _) = S.singleton i


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
