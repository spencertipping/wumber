{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | Affine transformations for things in general.
module Wumber.Affine where


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
import Text.Printf       (printf)

import Wumber.BoundingBox
import Wumber.ClosedComparable
import Wumber.Numeric
import Wumber.SymExpr
import Wumber.SymMath


-- | The class of values that have mappings to homogeneous coordinates.
class HomogeneousVector (v :: * -> *) where
  type Hom v :: * -> *
  hom   :: Num a        => v a -> Hom v a
  unhom :: Fractional a => Hom v a -> v a

instance HomogeneousVector V2 where
  type Hom V2 = V3
  hom   (V2 x y)   = V3 x y 1
  unhom (V3 x y h) = V2 (x/h) (y/h)

instance HomogeneousVector V3 where
  type Hom V3 = V4
  hom   (V3 x y z)   = V4 x y z 1
  unhom (V4 x y z h) = V3 (x/h) (y/h) (z/h)


-- | The class of objects that can be transformed using an affine matrix of type
--   'm'. For example, 'm' will be @M44@ (wrapped as @AffineM3@) to operate on
--   3D vectors.
class (HomogeneousVector v, AffineMatrix m v n) =>
      Affine a m v n | a -> m, a -> n where
  transform   :: m n -> a -> a

  untransform :: m n -> a -> a
  translate   :: v n -> a -> a
  rotate      :: v n -> n -> a -> a
  scale       :: v n -> a -> a

  untransform = transform . minvert
  translate   = transform . mtranslate
  scale       = transform . mscale
  rotate v    = transform . mrotate v


instance Floating a => Affine (V3 a) AffineM3 V3 a where
  transform (AM3 m) v = (hom v *! m) ^. _xyz

instance Floating a => Affine (V2 a) AffineM2 V2 a where
  transform (AM2 m) v = (hom v *! m) ^. _xy

instance (Traversable v, MonadZip v, Affine (v a) m v a, Bounded (v a),
          ClosedComparable a) =>
         Affine (BoundingBox (v a)) m v a where
  transform m b = of_points $ map (transform m) (corners b)


-- NOTE
-- We can transform symbolic quantities that are acting as vector functions. To
-- do this, we apply the transformation matrix inverse to their inputs and
-- rewrite the functions.
--
-- Sadly, we can't generalize over vector dimension and use 'vars'; there isn't
-- a way for us to pass enough type information down to the 'vars' invocation
-- since it's being consumed by 'toList'.

instance SymMathC f a =>
         Affine (SymMathV V2 f a) AffineM2 V2 (SymMath f a) where
  transform m (SymMathV v) = SymMathV $ v // v'
    where v' = zip [0..] $ toList $ transform (minvert m) v2

instance SymMathC f a =>
         Affine (SymMathV V3 f a) AffineM3 V3 (SymMath f a) where
  transform m (SymMathV v) = SymMathV $ v // v'
    where v' = zip [0..] $ toList $ transform (minvert m) v3


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
  deriving (Eq, Functor, Generic, Binary)

newtype AffineM2 a = AM2 { unAM2 :: M33 a }
  deriving (Eq, Functor, Generic, Binary)

instance Show a => Show (AffineM3 a) where
  show (AM3 (V4 a b c d)) = concatMap showrow [a, b, c, d]
    where showrow (V4 x y z t) = printf "%.16s  %.16s  %.16s  %.16s\n"
                                 (show x) (show y) (show z) (show t)

instance Show a => Show (AffineM2 a) where
  show (AM2 (V3 a b c)) = concatMap showrow [a, b, c]
    where showrow (V3 x y z) = printf "%.16s  %.16s  %.16s\n"
                               (show x) (show y) (show z)


instance Num a => Monoid    (AffineM2 a) where mempty         = AM2 identity
instance Num a => Monoid    (AffineM3 a) where mempty         = AM3 identity
instance Num a => Semigroup (AffineM2 a) where AM2 x <> AM2 y = AM2 (x !*! y)
instance Num a => Semigroup (AffineM3 a) where AM3 x <> AM3 y = AM3 (x !*! y)


instance Floating a => AffineMatrix AffineM2 V2 a where
  minvert             = AM2 . inv33 . unAM2
  mtranslate (V2 x y) = AM2 $ V3 (V3 1 0 0) (V3 0 1 0) (V3 x y 1)
  mscale     (V2 x y) = AM2 $ V3 (V3 x 0 0) (V3 0 y 0) (V3 0 0 1)
  mrotate v θ         = mtranslate (-v)
                        <> AM2 (V3 (V3 c (-s) 0) (V3 s c 0) (V3 0 0 1))
                        <> mtranslate (transform r' v)
                        <> error "FIXME: mrotate V2"
    where s  = sin θ
          c  = cos θ
          r' = AM2 (V3 (V3 c s 0) (V3 (-s) c 0) (V3 0 0 1))

instance Floating a => AffineMatrix AffineM3 V3 a where
  minvert                 = AM3 . inv44 . unAM3
  mtranslate (V3 x y z)   = AM3 $ V4 (V4 1 0 0 0) (V4 0 1 0 0)
                                     (V4 0 0 1 0) (V4 x y z 1)
  mscale     (V3 x y z)   = AM3 $ V4 (V4 x 0 0 0) (V4 0 y 0 0)
                                     (V4 0 0 z 0) (V4 0 0 0 1)
  mrotate    (V3 x y z) θ = AM3 $
    V4 (V4 (x*x*(1-c) + c)   (x*y*(1-c) - z*s) (x*z*(1-c) + y*s) 0)
       (V4 (x*y*(1-c) + z*s) (y*y*(1-c) + c)   (y*z*(1-c) - x*s) 0)
       (V4 (x*z*(1-c) - y*s) (y*z*(1-c) + x*s) (z*z*(1-c) + c)   0)
       (V4 0                 0                 0                 1)
    where s = sin θ
          c = cos θ
