{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}

module Cur where

import Linear
import Numeric.LinearAlgebra

data Unit = KM | M | CM | MM | UM

factor :: Fractional a => Unit -> a
factor KM = 1000
factor M  = 1
factor CM = 0.01
factor MM = 0.001
factor UM = 0.000001

km = KM
m  = M
cm = CM
mm = MM
μm = UM


instance Fractional a => Num (Unit -> a) where
  fromInteger x = \u -> fromInteger x * factor u

instance Fractional a => Fractional (Unit -> a) where
  fromRational x = \u -> fromRational x * factor u


v = V3 (5km) (6mm) (7m)
