{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Wumber.ConstraintSolver where

import Control.Monad
import Control.Monad.RWS
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Vector.Storable as V
import Data.Vector.Storable (Vector, (!))
import Numeric.GSL.Minimization

import Wumber.Constraint


-- | All independent variables used to calculate the given value.
deps :: CVal -> S.Set (VarID, N)
deps (CVar i v)            = S.singleton (i, v)
deps (CConst _)            = S.empty
deps (CLinear _ _ v)       = deps v
deps (CNonlinear xs _ _)   = S.unions (map deps xs)
deps (CNonlinearU v _ _)   = deps v
deps (CNonlinearB l r _ _) = deps l `S.union` deps r

eval :: Vector N -> CVal -> N
eval xs (CVar i _)            = xs ! i
eval _  (CConst x)            = x
eval xs (CLinear m b v)       = let x = eval xs v in m*x + b
eval xs (CNonlinear ops f _)  = f $ map (eval xs) ops
eval xs (CNonlinearU v f _)   = f (eval xs v)
eval xs (CNonlinearB l r f _) = eval xs l `f` eval xs r


constraints_deps :: [Constraint] -> S.Set (VarID, N)
constraints_deps = S.unions . map \case CEqual a b -> deps a `S.union` deps b
                                        CCostFn v  -> deps v

eval_constraints :: [Constraint] -> Vector N -> N
eval_constraints cs xs = L.foldl' (\t v -> t + each v) 0 cs
  where each (CEqual a b) = (eval xs a - eval xs b) ** 2
        each (CCostFn v)  = max 0 $ eval xs v


-- | The class of objects that can have constraint variables rewritten into
--   fixed values. Not all objects will preserve form when you do this; you
--   might start with constraint-friendly data structures that render themselves
--   into more optimized final types.
class Rewritable a b | a -> b where rewrite :: (CVal -> N) -> a -> b

instance              Rewritable    CVal     N  where rewrite = id
instance Functor f => Rewritable (f CVal) (f N) where rewrite = fmap


-- TODO: solve by substitution when we see usable 'CEqual' constraints
-- TODO: figure out what 'search_size' is for
solve :: Rewritable a b
      => N -> Int -> Constrained a -> (b, Vector N, [Constraint])
solve δ n m = (rewrite (eval solution) a, solution, cs)
  where (a, cs)     = evalRWS m () 0
        f           = eval_constraints cs
        vars        = constraints_deps cs
        start       = V.replicate (1 + fst (S.findMax vars)) 0 V.// S.toList vars
        search_size = V.replicate (1 + fst (S.findMax vars)) 1
        solution    = fst $ minimizeV NMSimplex2 δ n search_size f start
