{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | Models that support affine transformations.
module Wumber.ModelAffine where


import Control.Monad.Zip (MonadZip)
import Data.Binary       (Binary)
import Data.Foldable     (toList)
import Data.IntMap       (fromList)
import GHC.Generics      (Generic)
import Lens.Micro        ((&), (.~), (^.))
import Linear.Matrix     (M33, M44, (!*!), (*!), identity, inv33, inv44)
import Linear.V2         (V2(..))
import Linear.V3         (V3(..), _xy, _xyz)
import Linear.V4         (V4(..))

import Wumber.BoundingBox
import Wumber.ClosedComparable
import Wumber.Model
import Wumber.Numeric
import Wumber.Symbolic


-- | The class of objects that can be transformed using an affine matrix of type
--   'm'. Typically 'm' will be @M44 R@ or @M44 (Sym f R)@ to operate on 3D
--   vectors.

class AffineMatrix m v n => Affine a m v n | a -> m, a -> n where
  transform :: m n -> a -> a
  translate :: v n -> a -> a
  rotate    :: v n -> n -> a -> a
  scale     :: v n -> a -> a

  translate = transform . mtranslate
  scale     = transform . mscale
  rotate v  = transform . mrotate v


instance Floating a => Affine (V3 a) AffineM3 V3 a where
  transform (AM3 m) (V3 x y z) = (V4 x y z 1 *! m) ^. _xyz

instance Floating a => Affine (V2 a) AffineM2 V2 a where
  transform (AM2 m) (V2 x y) = (V3 x y 1 *! m) ^. _xy

instance (Traversable v, MonadZip v, Affine (v a) m v a, Bounded (v a),
          ClosedComparable a) =>
         Affine (BoundingBox (v a)) m v a where
  transform m b = of_points $ map (transform m) (corners b)

instance AlgConstraints f R => Affine (FRep V3 f) AffineM3 V3 R where
  transform m (FRep f b) = FRep (transform (fmap val m) f) (transform m b)


-- NOTE
-- We can transform symbolic quantities that are acting as vector functions. To
-- do this, we apply the transformation matrix inverse to their inputs and
-- rewrite the functions.
--
-- Sadly, we can't generalize over vector dimension and use 'vars'; there isn't
-- a way for us to pass enough type information down to the 'vars' invocation
-- since it's being consumed by 'fromList'.

instance AlgConstraints f a => Affine (SymV V2 f a) AffineM2 V2 (Sym f a) where
  transform m (SymV v) = SymV $ rewrite_vars v' v
    where v' = fromList $ zip [0..] $ toList $ transform (minvert m) v2

instance AlgConstraints f a => Affine (SymV V3 f a) AffineM3 V3 (Sym f a) where
  transform m (SymV v) = SymV $ rewrite_vars v' v
    where v' = fromList $ zip [0..] $ toList $ transform (minvert m) v3


-- | The class of matrices that offer affine constructors. We need this to
--   generalize over dimensionality.
--
--   NOTE
--   'mrotate' doesn't have a consistent meaning across dimensions. In 2D space
--   there's only one rotation possible, but this typeclass requires us to have
--   a vector representing the axis. As a convenience (or perhaps
--   inconvenience), the vector argument of 'mrotate' is the center of rotation
--   in 2D.

class Monoid (m n) =>
      AffineMatrix (m :: * -> *) (v :: * -> *) n | m -> v, v -> m where
  minvert    :: m n -> m n
  mtranslate :: v n -> m n
  mscale     :: v n -> m n
  mrotate    :: v n -> n -> m n

newtype AffineM3 a = AM3 { unAM3 :: M44 a }
  deriving (Show, Eq, Functor, Generic, Binary)

newtype AffineM2 a = AM2 { unAM2 :: M33 a }
  deriving (Show, Eq, Functor, Generic, Binary)


instance Num a => Semigroup (AffineM2 a) where AM2 x <> AM2 y = AM2 (x !*! y)
instance Num a => Semigroup (AffineM3 a) where AM3 x <> AM3 y = AM3 (x !*! y)

instance Num a => Monoid (AffineM2 a) where mempty = AM2 identity
instance Num a => Monoid (AffineM3 a) where mempty = AM3 identity


instance Floating a => AffineMatrix AffineM2 V2 a where
  minvert             = AM2 . inv33 . unAM2
  mtranslate (V2 x y) = AM2 $ V3 (V3 1 0 0) (V3 0 1 0) (V3 x y 1)
  mscale     (V2 x y) = AM2 $ V3 (V3 x 0 0) (V3 0 y 0) (V3 0 0 1)
  mrotate v θ         = mtranslate v
                        <> AM2 (V3 (V3 c (-s) 0) (V3 s c 0) (V3 0 0 1))
                        <> mtranslate (-v)
    where s = sin θ
          c = cos θ

instance Floating a => AffineMatrix AffineM3 V3 a where
  minvert                 = AM3 . inv44 . unAM3
  mtranslate (V3 x y z)   = AM3 $ V4 (V4 1 0 0 0) (V4 0 1 0 0) (V4 0 0 1 0) (V4 x y z 1)
  mscale     (V3 x y z)   = AM3 $ V4 (V4 x 0 0 0) (V4 0 y 0 0) (V4 0 0 z 0) (V4 0 0 0 1)
  mrotate    (V3 x y z) θ = AM3 $
    V4 (V4 (x*x*(1-c) + c)   (x*y*(1-c) - z*s) (x*z*(1-c) + y*s) 0)
       (V4 (x*y*(1-c) + z*s) (y*y*(1-c) + c)   (y*z*(1-c) - x*s) 0)
       (V4 (x*z*(1-c) - y*s) (y*z*(1-c) + x*s) (z*z*(1-c) + c)   0)
       (V4 0                 0                 0                 1)
    where s = sin θ
          c = cos θ