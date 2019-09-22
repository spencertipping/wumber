{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}

module Cur where

{-
import Data.Dimensions.SI
import Data.Metrology
import Data.Metrology.SI
import Data.Units.SI
import Data.Units.US
-}
import Linear
import Numeric.LinearAlgebra


data Coordinate = X | Y | Z
x = X
y = Y
z = Z

vec :: Num a => a -> Coordinate -> V3 a
vec n X = V3 n 0 0
vec n Y = V3 0 n 0
vec n Z = V3 0 0 n


plus'  x y u = x u + y u
minus' x y u = x u - y u
times' x y u = x u * y u
div'   x y u = x u / y u

-- TODO: why does abs' = (. abs) fail, even with the right type signature?
abs'    x u = abs (x u)
signum' x u = signum (x u)


type P = V3 Double

{-
instance (UnitFactorsOf u, Fractional a) => Num (u -> a) where
  fromInteger x u = fromInteger x * factor u
  (+) = plus'
  (-) = minus'
  (*) = times'
  abs = abs'
  signum = signum'

instance Fractional a => Fractional (Unit -> a) where
  fromRational x u = fromRational x * factor u
  (/) = div'
-}

instance Num a => Num (Coordinate -> V3 a) where
  fromInteger x c = vec (fromInteger x) c
  (+) = plus'
  (-) = minus'
  (*) = times'
  abs = abs'
  signum = signum'

instance Fractional a => Fractional (Coordinate -> V3 a) where
  fromRational x c = vec (fromRational x) c
  (/) = div'

instance Num a => Num (a -> a) where
  fromInteger n = (*) (fromInteger n)
  (+) = plus'
  (-) = minus'
  (*) = times'
  abs = abs'
  signum = signum'

instance Fractional a => Fractional (a -> a) where
  fromRational n = (*) (fromRational n)
  (/) = div'
