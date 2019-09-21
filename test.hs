{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}

module Main where

import Numeric.Units.Dimensional as ND
import Numeric.Units.Dimensional.SIUnits

import Prelude hiding ((*))

instance (Num a) => Num (Unit m t a -> Quantity t a) where
  fromInteger i = \d -> fromIntegral i *~ d

instance (Fractional a) => Fractional (Unit m t a -> Quantity t a) where
  fromRational r = \d -> fromRational r *~ d

mm :: Unit 'NonMetric DLength Float
mm = milli meter
mm² = mm ND.* mm

-- Hot damn, we might be onto something here
main :: IO ()
main = do
  putStrLn $ show (15mm  :: Quantity DLength Float)
  putStrLn $ show (15mm² :: Quantity (DLength * DLength) Float)
  putStrLn $ show (1 *~ (meter ND.* meter ND.* gram))
