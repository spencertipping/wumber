{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
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
import Data.List     (partition)
import Data.Maybe    (catMaybes)
import GHC.Generics  (Generic)
import Lens.Micro.TH (makeLenses)

import Wumber.MathFn
import Wumber.Numeric
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
--   - '_es_minimize': the current cost function, made of equations we couldn't
--     use to produce algebraic substitutions
--   - '_es_inconsistent': any constraints we failed to integrate because they
--     conflicted with other constraints
--   - '_es_amb': equations that can provide multiple substitutions, and we
--     haven't yet decided which one to commit to
--   - '_es_dofs': current degrees of freedom

data EquationSystem f a = ES { _es_subst        :: !(IntMap (SymMath f a)),
                               _es_substset     :: BS.BitSet,
                               _es_minimize     :: SplitCostFn f a,
                               _es_inconsistent :: [SymMath f a],
                               _es_amb          :: [Equation f a],
                               _es_dofs         :: BS.BitSet }
  deriving (Show, Eq, Generic, Binary)


-- | An empty equation system.
init_es :: EquationSystem f a
init_es = ES IM.empty BS.empty [] [] [] BS.empty


-- | A cost function partitioned across multiple independent subsets of a
--   variable space. We represent it this way so that we can invoke the
--   numerical solver on subsystems with minimal dimension.
type SplitCostFn f a = [SymMath f a]


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
--     once solved.
--   - 'EqMultivariate': the equation relates multiple quantities; solving for
--     any one of them won't yield a constant.
--
--   Some of these definitions refer to 'EquationSystem's even though we
--   determine an equation's type in isolation. We do this by applying a
--   system's substitutions to the equation 'SymMath' before characterizing it,
--   entangling the system into the equation up front.

-- TODO
-- 'EqUnivariate' isn't the whole story; some univariate equations can be solved
-- numerically even though the variable can't be isolated.

-- TODO
-- Sometimes we'll have a self-contained subsystem that is entangled with a
-- larger subsystem. It makes sense to partition by solving the smaller one
-- first, then propagating constants into the larger one.

data EquationType = EqConsistent
                  | EqInconsistent
                  | EqMinimize
                  | EqUnivariate
                  | EqMultivariate
  deriving (Show, Eq, Ord, Bounded, Enum, Generic, Binary)


makeLenses ''EquationSystem
makeLenses ''Equation


-- | Adds a constraint to an equation system, where the constraint is a value to
--   be set to zero. Mechanically, our goal is to use the new constraint to
--   build new variable substitutions. Failing that, we try to add it to the
--   list of quantities to minimize.
constrain :: (Invertible (SymMath f a), SymMathC f a, Delta a)
          => SymMath f a -> EquationSystem f a -> EquationSystem f a
constrain c es@(ES s ss f i a d) = case _eq_type e of
  EqConsistent   -> es
  EqInconsistent -> ES s ss f (c:i) a d
  EqMinimize     -> ES s ss (add_costfn (_eq_q e) f) i a d
  EqMultivariate -> add_multivariate e es
  _              -> add_univariate e es
  where e = equation (c //: s)


-- | Constructs an 'Equation' from the given 'SymMath' quantity by attempting to
--   isolate each one of its variables. The 'SymMath' quantity is converted to
--   an implicit equation by setting it to zero.
equation :: (Invertible (SymMath f a), SymMathC f a, Delta a)
         => SymMath f a -> Equation f a
equation m = EQN m t (BS.fromList $ map fst isos) (IM.fromList isos)
  where vs        = BS.toList $ vars_in m
        isos      = catMaybes $ map try_iso vs
        try_iso v = (v, ) <$> isolate v m
        v         = val_of m
        t         = if | Just v' <- v, abs v' <= δ 1 -> EqConsistent
                       | Just _ <- v                 -> EqInconsistent
                       | null isos                   -> EqMinimize
                       | length isos == 1            -> EqUnivariate
                       | otherwise                   -> EqMultivariate


-- | Adds a constraint to a 'SplitCostFn', un-splitting newly entangled
--   subsystems if necessary. The constraint is squared before summing it into a
--   combined cost function.
add_costfn :: SymMathC f a => SymMath f a -> SplitCostFn f a -> SplitCostFn f a
add_costfn c fs = i' : o
  where vs     = vars_in c
        (i, o) = partition (BS.intersects vs . vars_in) fs
        i'     = c**2 + sum i


-- | Applies a substitution map to a 'SplitCostFn' efficiently.
subst_costfn :: SymMathC f a
             => BS.BitSet -> IntMap (SymMath f a)
             -> SplitCostFn f a -> SplitCostFn f a
subst_costfn b m fs = foldr add_costfn o $ map (//: m) i
  where (i, o) = partition (BS.intersects b . vars_in) fs


-- | Adds a multivariate equation to an 'EquationSystem'. If the equation
--   provides a substitution for any existing degree of freedom, we choose one
--   arbitrarily. Otherwise we enqueue the equation onto the '_es_amb' list.
add_multivariate :: (Invertible (SymMath f a), SymMathC f a, Delta a)
                 => Equation f a -> EquationSystem f a -> EquationSystem f a
add_multivariate e es@(ES s ss f i a d)
  | v:_ <- vs = add_subst v (_eq_subst e IM.! v) es
  | otherwise = ES s ss f i (e:a) (BS.union d $ _eq_substset e)
  where vs = BS.toList $ BS.intersect d (_eq_substset e)


-- | Adds a univariate equation to an 'EquationSystem'. We do this by solving
--   the equation, either numerically or algebraically, and adding a
--   substitution.
add_univariate :: (Invertible (SymMath f a), SymMathC f a, Delta a)
               => Equation f a -> EquationSystem f a -> EquationSystem f a
add_univariate e = add_subst v q where [(v, q)] = IM.toList (_eq_subst e)


-- | Applies a new substitution to an 'EquationSystem'. If the 'EquationSystem'
--   has ambivalent equations, then this may trigger a cascade of substitutions.
--
--   If you try to substitute the same variable multiple times, we'll derive a
--   new equation by setting the substitutions equal to each other.

add_subst :: (Invertible (SymMath f a), SymMathC f a, Delta a)
          => VarID -> SymMath f a -> EquationSystem f a -> EquationSystem f a
add_subst v q es@(ES s ss f i a d)
  | Just q' <- IM.lookup v s = constrain (q - q') es
  | otherwise                = foldr constrain es0 (map _eq_q ai)
  where es0      = ES s' ss' f' i ao (BS.unions $ map (vars_in . _eq_q) ao)
        s'       = IM.insert v q $ IM.map (// [(v, q)]) s
        ss'      = BS.union ss (BS.singleton v)
        f'       = subst_costfn ss' s' f
        (ai, ao) = partition (BS.member v . _eq_substset) a
