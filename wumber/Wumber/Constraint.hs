{-# LANGUAGE MonoLocalBinds #-}
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


import Control.Monad.RWS (RWS, gets, modify', tell)
import Data.Binary       (Binary)
import Data.Either       (lefts, rights)
import Data.Foldable     (toList)
import Data.IntMap       (IntMap, insert, (!?))
import Data.Maybe        (isJust)
import GHC.Generics      (Generic(..))
import Lens.Micro        (_1, _2, (%~), (&))

import qualified Data.IntMap as IM
import qualified Data.IntSet as IS

import Wumber.ClosedComparable
import Wumber.Numeric
import Wumber.Symbolic
import Wumber.SymbolicAlgebra


-- | A value being set to zero, symbolically or, failing that, numerically.
type CVal f = Sym f R


-- | 'Constrained' is a monad that keeps track of 'Var' IDs and collects
--   equivalences, quantities to minimize, and initial values. Generally you'd
--   use one such monad per independent constraint system within a project,
--   although it's fine to combine many into one: Wumber will figure out when
--   you have separable subsystems and solve them independently.
type Constrained f = RWS () [Either (VarID, R) (CVal f)] (VarID, IntMap (CVal f))


-- | Create a new constrained variable initialized to the specified value.
cvar :: FConstraints f R => R -> Constrained f (CVal f)
cvar init = do id <- gets fst
               modify' (_1 %~ (+ 1))
               tell [Left (id, init)]
               return $ var id

-- | A multidimensional variant of 'cvar'.
cvars :: (FConstraints f R, Traversable t) => t R -> Constrained f (t (CVal f))
cvars = mapM cvar


-- | Sets two constrained quantities equal to each other. When the two
--   quantities yield isolatable terms, adds entries to the substitution map.
set_equal :: AlgConstraints f R => CVal f -> CVal f -> Constrained f (CVal f)
set_equal a b = do m <- gets snd
                   let v = rewrite_vars m (a - b)
                   update_rewrite_table v
                   tell [Right v]
                   return v

-- | Sets one constrained quantity to be less than the other.
set_below :: AlgConstraints f R => CVal f -> CVal f -> Constrained f (CVal f)
set_below a b = do m <- gets snd
                   let v = rewrite_vars m $ (b - a) `upper` 0
                   tell [Right v]
                   return v


-- | Isolates local variables to create new entries in the substitution table.
--   This isn't an exact science; the main purpose is to pre-simplify the set of
--   variables before we send the resulting constraints off to modules like
--   'ConstraintSplit' and 'ConstraintSimplify'.
update_rewrite_table :: AlgConstraints f R => CVal f -> Constrained f ()
update_rewrite_table v = do
  m <- gets snd
  let vars = IS.filter (not . flip IM.member m) (vars_in v) & IS.toList
      isos = zip vars (map (isolate v 0) vars)
             & filter (isJust . snd)
             & map (\(a, Just b) -> (a, normalize b))
  modify' $ _2 %~ IM.union (IM.fromList isos)


-- | Constraint equivalence. The premise is that we can reduce each constraint
--   down to one or more scalar values that describe its out-of-whackness. The
--   solver attempts to set these values to zero.
class AlgConstraints f R => CEq f a | a -> f where
  infix 4 =-=; (=-=) :: a -> a -> Constrained f (CVal f)
  infix 4 <-=; (<-=) :: a -> a -> Constrained f (CVal f)
  infix 4 >-=; (>-=) :: a -> a -> Constrained f (CVal f)
  (>-=) = flip (<-=)

instance {-# OVERLAPPABLE #-} AlgConstraints f R => CEq f (CVal f) where
  (=-=) = set_equal
  (<-=) = set_below

instance {-# OVERLAPPABLE #-} (Foldable f, CEq t a) => CEq t (f a) where
  a =-= b = sum <$> (sequence $ zipWith (=-=) (toList a) (toList b))
  a <-= b = sum <$> (sequence $ zipWith (<-=) (toList a) (toList b))
