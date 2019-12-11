{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | A library to manage potentially large systems of equations, reducing
--   dimensionality eagerly as new relations are added.
module Wumber.EquationSystem where


import Data.Binary   (Binary)
import Data.IntMap   (IntMap(..))
import Data.Maybe    (catMaybes)
import GHC.Generics  (Generic)
import Lens.Micro.TH (makeLenses)

import Wumber.MathFn
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
--   - '_es_substset': a bitset of the keys in '_es_subst'
--   - '_es_minimize': partial equations we should end up trying to minimize,
--     but that didn't provide substitutions
--   - '_es_amb': equations that can provide multiple substitutions, and we
--     haven't yet decided which one to commit to
--   - '_es_to_rewrite': variables that would be useful to rewrite

data EquationSystem f a = ES { _es_subst      :: !(IntMap (SymMath f a)),
                               _es_substset   :: BS.BitSet,
                               _es_minimize   :: [SymMath f a],
                               _es_amb        :: [Equation f a],
                               _es_to_rewrite :: BS.BitSet }
  deriving (Show, Eq, Generic, Binary)


-- | A single equation that knows which variables can be isolated.
data Equation f a = EQN { _eq_q        :: !(SymMath f a),
                          _eq_type     :: EquationType,
                          _eq_substset :: BS.BitSet,
                          _eq_subst    :: IntMap (SymMath f a) }
  deriving (Show, Eq, Generic, Binary)


-- | An equation's impact on a system. 'constrain' uses this to figure out how
--   to use new equations as they are added.
--
--   An equation can be any of the following:
--
--   - 'EqConsistent': the equation provides no new information, but doesn't
--     conflict with other equations.
--   - 'EqInconsistent': the equation provides no new information, but is
--     contradicted by another equation in the system.
--   - 'EqMinimize': the equation has variables, but none of them can be
--     isolated; its only purpose is to act as a numerical cost function.
--   - 'EqUnivariate': the equation will specify the value of a single variable
--     once solved (either algebraically or numerically).
--   - 'EqUniMultivariate': the equation refers to multiple variables, but only
--     one of them can be isolated.
--   - 'EqMultivariate': the equation relates multiple quantities; solving for
--     any one of them won't yield a constant.
--
--   Some of these definitions refer to 'EquationSystem's even though we
--   determine an equation's type in isolation. We do this by applying a
--   system's substitutions to the equation 'SymMath' before characterizing it,
--   entangling the system into the equation up front.

data EquationType = EqConsistent
                  | EqInconsistent
                  | EqMinimize
                  | EqUnivariate
                  | EqUniMultivariate
                  | EqMultivariate
  deriving (Show, Eq, Ord, Bounded, Enum, Generic, Binary)


makeLenses ''EquationSystem
makeLenses ''Equation


-- | Constructs an 'Equation' from the given 'SymMath' quantity by attempting to
--   isolate each one of its variables. The 'SymMath' quantity is converted to
--   an implicit equation by setting it to zero.
equation :: (Invertible (SymMath f a), SymMathC f a, SymVal a a)
         => SymMath f a -> Equation f a
equation m = EQN m t (BS.fromList $ map fst isos) (IM.fromList isos)
  where vs        = BS.toList $ vars_in (unMath m)
        isos      = catMaybes $ map try_iso vs
        try_iso v = (v, ) <$> isolate v m
        v         = val_of m
        t         = if | Just 0 <- v      -> EqConsistent
                       | Just _ <- v      -> EqInconsistent
                       | null isos        -> EqMinimize
                       | length vs == 1   -> EqUnivariate
                       | length isos == 1 -> EqUniMultivariate
                       | otherwise        -> EqMultivariate


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
--   If the equation provides multiple substitutions, e.g. @x + y + z = 0@, then
--   the right move is a little less clear.
--
--   For example, suppose we encounter @x + y + z = 0@ as the first equation in
--   a system. We don't want to commit to a rewrite just yet because the next
--   equation might be @y = abs z@. Had we chosen @y = -x - z@, we would now
--   have to backtrack over the first equation in order to get a rewrite rule
--   from the second. (Basically, there's only one rewrite we can get from a
--   lossy function like @y = abs z@.)
--
--   I don't want to go to a huge amount of effort for cases like this because I
--   don't think they're very common; I think all we need to do is handle the
--   worst of it by deferring any equations whose rewrites don't yet have any
--   impact.

constrain :: (Invertible (SymMath f a), SymMathC f a, SymVal a a)
          => SymMath f a -> EquationSystem f a -> EquationSystem f a
constrain c (ES s ss m a r) = ES s ss m a r
  where e = equation (c //: s)
