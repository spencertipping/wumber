{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | Kinematic graphs defined as compositions of affine transformations. The
--   general idea is that kinematic linkages relate the endpoint coordinate
--   space to the transformation parameters of the linkages, which we represent
--   using an 'EquationSystem'.
--
--   Generalized affine transformations give you more degrees of freedom than
--   mechanical systems have; for example, there aren't any real kinematic
--   linkages that produce scaling or shearing matrices. However, we don't limit
--   the set of matrices you can use to build kinematic graphs because we aren't
--   fascists. Your warranty may be void if your kinematic linkages scale stuff,
--   but we figure you have your reasons.

module Wumber.Kinematic where


import Data.Binary  (Binary)
import GHC.Generics (Generic)

import Wumber.Affine
import Wumber.EquationSystem
import Wumber.Functionable
import Wumber.Numeric
import Wumber.SymAlgebra
import Wumber.SymMath


-- | Type constraints for 'Kinematic's.
type KinematicC (m :: * -> *) (v :: * -> *) f a =
  (EquationC f a,
   Foldable m,
   Foldable v,
   Num (v (SymMath f a)),
   Num (m (SymMath f a)),
   HomogeneousVector v,
   AffineMatrix m v (SymMath f a),
   Affine (v (SymMath f a)) m v (SymMath f a),
   Affine (m (SymMath f a)) m v (SymMath f a))


-- | A condensed kinematic graph; that is, a graph with an end transform and
--   inherited loop constraints.
data Kinematic m f a = KG { _kg_transform   :: m (SymMath f a),
                            _kg_constraints :: EquationSystem f a,
                            _kg_priors      :: [Kinematic m f a] }
  deriving (Generic)

deriving instance (Binary f, Binary a, Binary (m (SymMath f a))) =>
                  Binary (Kinematic m f a)

deriving instance (Eq f, Eq a, Eq (m (SymMath f a))) =>
                  Eq (Kinematic m f a)

deriving instance (FnShow f, Show a, Show (m (SymMath f a))) =>
                  Show (Kinematic m f a)

instance KinematicC m v f a =>
         Affine (Kinematic m f a) m v (SymMath f a) where
  transform m g@(KG t c _) = KG (transform m t) c [g]


-- | The origin kinematic system; that is, one with no constraints and no
--   linkages. Its position is fixed at the origin and its transformation matrix
--   is the identity.
init_kg :: KinematicC m v f a => Kinematic m f a
init_kg = KG mempty init_es []


-- | Joins two kinematic systems by unifying a projection of their endpoints.
--   For example, 'join_by' 'end_transform' will set the two systems to end at
--   exactly the same place; 'join_by' 'end_position' allows the orientations to
--   differ but unifies the positions.
--
--   'join_by' @f@ @a@ @b@ inherits its transformation matrix from @a@, which is
--   relevant when the joint is not fully constrained. @b@'s transformation
--   matrix is lost to the extent that it isn't unified with @a@'s.

join_by :: (Foldable t, Num (t (SymMath f a)), KinematicC m v f a) =>
           (m (SymMath f a) -> t (SymMath f a)) ->
           Kinematic m f a -> Kinematic m f a -> Kinematic m f a
join_by f a@(KG ta sa _) b@(KG tb sb _) = KG ta s' [a, b]
  where s' = foldr constrain (merge sa sb) (f tb - f ta)


-- | The ending transform of a kinematic linkage, which is just the linkage's
--   transformation matrix.
end_transform :: Kinematic m f a -> m (SymMath f a)
end_transform (KG t _ _) = t


-- | The orientation-free ending position of a kinematic linkage.
end_position :: KinematicC m v f a => Kinematic m f a -> v (SymMath f a)
end_position g = transform (end_transform g) 0
