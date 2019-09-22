{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}

module Cur where

data Vec = Vec { x :: !Double,
                 y :: !Double,
                 z :: !Double }
  deriving (Show, Eq, Ord)

data Unit = KM | M | CM | MM | UM

factor :: Unit -> Double
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


instance Num (Unit -> Double) where
  fromInteger x = \u -> fromInteger x * factor u

instance Fractional (Unit -> Double) where
  fromRational x = \u -> fromRational x * factor u


v = Vec (5km) (6mm) (7m)

instance Num Vec where
  (Vec x1 y1 z1) + (Vec x2 y2 z2) = Vec (x1+x2) (y1+y2) (z1+z2)
  (Vec x1 y1 z1) - (Vec x2 y2 z2) = Vec (x1-x2) (y1-y2) (z1-z2)
