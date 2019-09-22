{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}

module Cur where

import Data.Dimensions.SI
import Data.Metrology
import Data.Units.SI
import Data.Units.SI.Prefixes
import Data.Units.US
import Language.Haskell.Interpreter
import Linear hiding (zero)
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

vec :: Num n => Qu d l n -> Coordinate -> V3 (Qu d l n)
vec n X = V3 n zero zero
vec n Y = V3 zero n zero
vec n Z = V3 zero zero n


plus'  x y u = x u + y u
minus' x y u = x u - y u
times' x y u = x u * y u
div'   x y u = x u / y u

-- TODO: why does abs' = (. abs) fail, even with the right type signature?
abs'    x u = abs (x u)
signum' x u = signum (x u)


type Len = Qu '[ 'F Length One] 'DefaultLCSU Double

km = Kilo :@ Meter
m  = Meter
cm = Centi :@ Meter
mm = Milli :@ Meter
μm = Micro :@ Meter
nm = Nano  :@ Meter

kg = Kilo :@ Gram
g  = Gram
mg = Milli :@ Gram
μg = Micro :@ Gram


type P = V3 Double


instance (
  Subset (CanonicalUnitsOfFactors (UnitFactorsOf unit))
         (CanonicalUnitsOfFactors (LookupList dim 'DefaultLCSU)),
  Subset (CanonicalUnitsOfFactors (LookupList dim 'DefaultLCSU))
         (CanonicalUnitsOfFactors (UnitFactorsOf unit)),
  UnitFactor (LookupList dim 'DefaultLCSU),
  Unit unit, dim ~ DimFactorsOf (DimOfUnit unit), Fractional n) =>
  Num (unit -> Qu dim 'DefaultLCSU n) where

  fromInteger x u = fromInteger x % u

instance (
  Subset (CanonicalUnitsOfFactors (UnitFactorsOf unit))
         (CanonicalUnitsOfFactors (LookupList dim 'DefaultLCSU)),
  Subset (CanonicalUnitsOfFactors (LookupList dim 'DefaultLCSU))
         (CanonicalUnitsOfFactors (UnitFactorsOf unit)),
  UnitFactor (LookupList dim 'DefaultLCSU),
  Unit unit, dim ~ DimFactorsOf (DimOfUnit unit), Fractional n) =>
  Fractional (unit -> Qu dim 'DefaultLCSU n) where

  fromRational x u = fromRational x % u


instance Num a => Num (Coordinate -> V3 (Qu d l a))

instance Fractional a => Fractional (Coordinate -> V3 (Qu d l a))


-- TODO: make this work
-- Right now it fails because Qu ... isn't a function; I think we need a
-- typeclass that's aware of vector-like things
-- v = (10nm |+| 15nm) x :: V3 Len
