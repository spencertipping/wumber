{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Main where

import Control.Monad
import GHC.Float
import Graphics.Gloss
import Linear.Metric
import Linear.V2
import Linear.V3
import System.Clock
import System.IO
import Text.Printf


data Implicit = ISphere !(V3 Double)
              | Intersect [Implicit]
              | Union     [Implicit]
              | Negate    Implicit

for = flip map
fi  = fromIntegral
f2d = float2Double


eval :: V3 Double -> Implicit -> Double
eval v (ISphere l)    = distance v l - 1
eval v (Intersect xs) = foldl1 max $ map (eval v) xs
eval v (Union xs)     = foldl1 min $ map (eval v) xs
eval v (Negate x)     = negate $ eval v x

time_ns :: IO Int
time_ns = fromInteger <$> toNanoSecs <$> getTime Realtime


-- TODO
render :: Implicit -> Picture
render m = Blank


model :: Float -> Implicit
model t = Union [ISphere (V3 (sin $ f2d t) 0 0),
                 ISphere (V3 (cos $ f2d t) 0 0)]


main :: IO ()
main =
  animate (InWindow "implicit" (1920, 1080) (100, 100))
          (makeColor 0.1 0.1 0.1 1)
          (render . model)
