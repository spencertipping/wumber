{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}

{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | Functions to reduce 'EquationSystem's to concrete values.
module Wumber.EquationSolve where


import Data.Binary              (Binary)
import Data.IntMap              (IntMap)
import Data.Maybe               (fromJust)
import GHC.Generics             (Generic)
import Numeric.GSL.Minimization (minimizeVD, MinimizeMethodD(..))

import Wumber.EquationSystem
import Wumber.Fingerprint
import Wumber.Numeric
import Wumber.SymDerivative
import Wumber.SymExpr
import Wumber.SymJIT
import Wumber.SymMath

import qualified Data.IntMap          as IM
import qualified Data.Vector.Storable as VS
import qualified Wumber.BitSet        as BS


-- | BFGS settings for the numerical solver.
data BFGSSettings = BFGSS { _bfgs_precision  :: Double,
                            _bfgs_iterations :: Int,
                            _bfgs_first_step :: Double,
                            _bfgs_tolerance  :: Double }
  deriving (Show, Eq, Generic, Binary)


default_settings :: BFGSSettings
default_settings = BFGSS (δ 1) 1048576 1 0.1


-- | Reduces a system of equations to concrete values using a mixture of
--   algebraic and numerical methods. If your equation system is underspecified,
--   then you'll get an incomplete solution.
solve :: SymMathC f R
      => BFGSSettings -> IntMap R -> EquationSystem f R -> IntMap R
solve ss ivs (ES s _ m _ a _) = IM.union mins s'
  where mins = IM.unions $ map (minimize ss ivs) m
        s'   = IM.map (eval (mins IM.!)) s


-- | Minimizes the solution error for the specified cost function by delegating
--   to the GSL BFGS minimizer.
minimize :: SymMathC f R
         => BFGSSettings -> IntMap R -> SymMath f R -> IntMap R
minimize (BFGSS prec iter fs tol) ivs m = IM.map (s VS.!) cs
  where cs   = IM.fromList $ zip (BS.toList (vars_in m)) [0..]
        csi  = IM.fromList $ zip [0..] $ BS.toList (vars_in m)
        m'   = m //: IM.map var cs
        ivs' = VS.generate (BS.size (vars_in m)) ((ivs IM.!) . (csi IM.!))

        ds     = map (jit . derivative m') $ IM.keys csi
        (s, _) = minimizeVD VectorBFGS2 prec iter fs tol
                 (jit m')
                 (\v -> VS.fromListN (length ds) $ map ($! v) ds)
                 ivs'
