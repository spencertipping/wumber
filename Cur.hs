{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}

module Cur where

import Data.Dimensions.SI
import Data.Metrology
import Data.Metrology.SI
import Data.Units.SI
import Data.Units.US
import Language.Haskell.Interpreter
import Linear
import Numeric.LinearAlgebra


say :: String -> Interpreter ()
say = liftIO . putStrLn

doit :: Interpreter [Int]
doit = do
  setImportsQ [("Prelude", Nothing)]
  interpret "5:[]" (as :: [Int])

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


m  = Meter
cm = 0.01m
mm = 0.001m :: Double
μm = 0.001mm :: Double


type P = V3 Double

instance (Unit u, Fractional a) => Num (u -> a) where
  fromInteger x u = fromInteger x
  (+) = plus'
  (-) = minus'
  (*) = times'
  abs = abs'
  signum = signum'

instance (Unit u, Fractional a) => Fractional (u -> a) where
  fromRational x u = fromRational x
  (/) = div'

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
