{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | Conversions between various semantically equivalent forms of numerical
--   vectors. This simplifies library interoperability.
--
--   'V2', 'V3', and 'V4' are our base types; most interoperability is specified
--   in terms of those because that's where most of Wumber's computation
--   happens.

-- TODO
-- Use TemplateHaskell to make this module less awful?

module Wumber.VectorConversion where


import Linear.V2 (V2(..))
import Linear.V3 (V3(..))
import Linear.V4 (V4(..))

import qualified Data.Vector           as V
import qualified Data.Vector.Storable  as VS
import qualified Numeric.LinearAlgebra as LA


-- | Specifies that we can convert vectors from type 'a' to 'b'.
class VectorConversion a b where vconvert :: a -> b


instance VectorConversion (V2 Double) (V.Vector Double) where
  vconvert (V2 x y) = V.fromList [x, y]
  {-# INLINE vconvert #-}

instance VectorConversion (V2 Double) (VS.Vector Double) where
  vconvert (V2 x y) = VS.fromList [x, y]
  {-# INLINE vconvert #-}

instance VectorConversion (V.Vector Double) (V2 Double) where
  vconvert v = V2 (v V.! 0) (v V.! 1)
  {-# INLINE vconvert #-}

instance VectorConversion (VS.Vector Double) (V2 Double) where
  vconvert v = V2 (v VS.! 0) (v VS.! 1)
  {-# INLINE vconvert #-}


instance VectorConversion (V3 Double) (V.Vector Double) where
  vconvert (V3 x y z) = V.fromList [x, y, z]
  {-# INLINE vconvert #-}

instance VectorConversion (V3 Double) (VS.Vector Double) where
  vconvert (V3 x y z) = VS.fromList [x, y, z]
  {-# INLINE vconvert #-}

instance VectorConversion (V.Vector Double) (V3 Double) where
  vconvert v = V3 (v V.! 0) (v V.! 1) (v V.! 2)
  {-# INLINE vconvert #-}

instance VectorConversion (VS.Vector Double) (V3 Double) where
  vconvert v = V3 (v VS.! 0) (v VS.! 1) (v VS.! 2)
  {-# INLINE vconvert #-}


instance VectorConversion (V4 Double) (V.Vector Double) where
  vconvert (V4 x y z t) = V.fromList [x, y, z, t]
  {-# INLINE vconvert #-}

instance VectorConversion (V4 Double) (VS.Vector Double) where
  vconvert (V4 x y z t) = VS.fromList [x, y, z, t]
  {-# INLINE vconvert #-}

instance VectorConversion (V.Vector Double) (V4 Double) where
  vconvert v = V4 (v V.! 0) (v V.! 1) (v V.! 2) (v V.! 3)
  {-# INLINE vconvert #-}

instance VectorConversion (VS.Vector Double) (V4 Double) where
  vconvert v = V4 (v VS.! 0) (v VS.! 1) (v VS.! 2) (v VS.! 3)
  {-# INLINE vconvert #-}
