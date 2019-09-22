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

fz :: Foo n => n
fz = fromNum 0

vec :: Foo n => n -> Coordinate -> V3 n
vec n X = V3 n fz fz
vec n Y = V3 fz n fz
vec n Z = V3 fz fz n


plus'  x y u = x u + y u
minus' x y u = x u - y u
times' x y u = x u * y u
div'   x y u = x u / y u

-- TODO: why does abs' = (. abs) fail, even with the right type signature?
abs'    x u = abs (x u)
signum' x u = signum (x u)


type N n = Qu '[] 'DefaultLCSU n
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


class Foo a where
  fromNum :: Num b => b -> a

instance Foo a => Foo (Coordinate -> a) where
  fromNum x c = fromNum $ vec (fromNum x) c

instance Fractional n => Foo (N n) where
  fromNum x = fromNum x % (Meter :/ Meter)

instance Fractional a => Foo (V3 a) where
  fromNum x = V3 (fromNum x) (fromNum x) (fromNum x)

instance (
  Subset (CanonicalUnitsOfFactors (UnitFactorsOf unit))
         (CanonicalUnitsOfFactors (LookupList dim 'DefaultLCSU)),
  Subset (CanonicalUnitsOfFactors (LookupList dim 'DefaultLCSU))
         (CanonicalUnitsOfFactors (UnitFactorsOf unit)),
  dim ~ '[],
  dim ~ DimFactorsOf (DimOfUnit unit),
  UnitFactor (LookupList dim 'DefaultLCSU),
  Unit unit, Foo a) => Foo (unit -> a) where

  fromNum x u = fromNum $ fromNum x % u

instance Foo a => Num a where
  fromInteger x = fromNum (fromInteger x)

instance Foo a => Fractional a where
  fromRational x = fromNum (fromRational x)

{-
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
-}

instance Num a => Num (Coordinate -> V3 (Qu d l a))

instance Fractional a => Fractional (Coordinate -> V3 (Qu d l a))


-- TODO: make this work
-- Right now it fails because Qu ... isn't a function; I think we need a
-- typeclass that's aware of vector-like things
-- v = (10nm |+| 15nm) x :: V3 Len
