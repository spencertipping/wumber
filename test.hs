{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Numeric.Units.Dimensional as ND
import Numeric.Units.Dimensional.SIUnits

data Dim = MM | CM deriving Show
data Len = L !Dim !Float deriving Show

instance Num Len where
  (L d x) + (L _ y) = L d (x Prelude.+ y)
  (L d x) - (L _ y) = L d (x Prelude.- y)
  (L d x) * (L _ y) = L d (x Prelude.* y)

instance Num (Dim -> Len) where
  fromInteger i = \d -> L d (fromIntegral i)

mm = MM
cm = CM

main :: IO ()
main = do
  let l = 5mm :: Len
  putStrLn $ show l
  putStrLn $ show (meter)
