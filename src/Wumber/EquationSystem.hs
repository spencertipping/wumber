{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | A library to manage potentially large systems of equations, reducing
--   dimensionality eagerly as new relations are added.
module Wumber.EquationSystem where


import Data.Binary  (Binary)
import Data.IntMap  (IntMap(..))
import GHC.Generics (Generic)

import Wumber.SymAlgebra
import Wumber.SymExpr
import Wumber.SymMath

import qualified Data.IntMap   as IM
import qualified Wumber.BitSet as BS


-- | A system of equations that keeps track of algebraic equivalences and
--   subsystem independence. Tracking and rewriting are done incrementally.
--
---  We store the following:
--
--   - '_es_subst': substitutions we can apply to variables
--   - '_es_minimize': partial equations we should end up trying to minimize,
--     but that didn't provide substitutions
--   - '_es_amb': equations that refer to multiple variables

data EquationSystem f a = ES { _es_subst    :: !(IntMap (SymMath f a)),
                               _es_minimize :: [SymMath f a],
                               _es_amb      :: [SymMath f a] }
  deriving (Show, Eq, Generic, Binary)


-- | Adds a constraint to an equation system, where the constraint is a value to
--   be set to zero. Mechanically, our goal is to use the new constraint to
--   build new variable substitutions -- but this process isn't entirely
--   straightforward.
--
--   First, we rewrite the constraint using existing substitutions. At this
--   point it will reduce to one of several things:
--
--   1. A constant, in which case the constraint is redundant
--   2. A univariate partial equation, which provides a quantity to minimize
--   3. A univariate total equation, which provides a constant substitution
--   4. A multivariate equation, which may provide a substitution
--
--   For case (1) we drop the constraint. Cases (2) and (3) are similar, but
--   handled differently. Case (2) doesn't yield a value for a variable, but
--   does constrain it. We're forced to add it to the list of quantities to
--   minimize; we can't use it in any algebraic way.
--
--   Case (3) provides a constant substitution for a variable whether or not the
--   equation can be solved algebraically. If it can't, we delegate to the
--   numerical solver; but in either case we'll end up with a new substitution
--   for the variable.
--
--   Case (4) is subtle and deserves some discussion.
--
--   If the multivariate equation can be solved for only one of its variables,
--   then we do that and add a substitution. Given our simplistic computer
--   algebra system, it's very unlikely that any substitution down the line is
--   going to transform an un-isolatable variable into something more useful.
--
--   If the equation provides multiple substitutions, e.g. @x + y = 0@, then we
--   have a few ways forwards:
--
--   1. Commit to a substitution that maximizes some measure of helpfulness
--   2. Note possible substitutions and enqueue the equation for later use
--   3. Commit to a substitution, but note the fact that we can invert it
--
--   (2) is a performance risk because we'll end up with things we can't reduce
--   despite having obvious ways to do so. The real choice is between (1) and
--   (3) -- and the operative difference is whether we want to support
--   backtracking.
--
--   TODO: more figuring this out

constrain :: SymMathC f a
          => SymMath f a -> EquationSystem f a -> EquationSystem f a
constrain c (ES s m a) = ES s m a
  where c' = c //: s
