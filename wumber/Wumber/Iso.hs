{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Wumber.Iso where

import Control.DeepSeq
import Control.Monad.Identity
import Control.Monad.RWS.Strict
import Control.Monad.ST
import Control.Parallel.Strategies
import Data.Array
import Data.Array.MArray
import Data.Array.ST
import Data.Foldable
import Data.Function
import Data.List
import GHC.Float
import Graphics.Gloss.Data.Color
import Lens.Micro
import Lens.Micro.TH
import Linear.Matrix
import Linear.Metric
import Linear.V2
import Linear.V3
import Linear.V4
import Linear.Vector
import System.IO
import System.Random

import Wumber.BoundingBox
import Wumber.Cursor
import Wumber.Element


-- TODO
-- Replace all of this with a dual contouring implementation, e.g.
-- https://www.boristhebrave.com/2018/04/15/dual-contouring-tutorial/
-- https://www.mattkeeter.com/projects/contours/


type Iso = V3 Double -> Double

sphere :: V3 Double -> Iso
sphere l v = 1 - distance v l

cube :: BB3D -> Iso
cube (BB (V3 x1 y1 z1) (V3 x2 y2 z2)) (V3 x y z) =
  foldl1 min [ x - x1, x2 - x, y - y1, y2 - y, z - z1, z2 - z ]


iunion     f g v = max (f v) (g v)
iintersect f g v = min (f v) (g v)
inegate    f v   = negate (f v)


ε :: Double
ε = 1e-8


pmap :: NFData b => (a -> b) -> [a] -> [b]
pmap f = withStrategy (parListChunk 64 rdeepseq) . map f


instance NFData Element where
  rnf (Multi _ xs)          = rnf xs
  rnf (Shape _ _ vs)        = rnf vs
  rnf (Replicate _ _ _ _ v) = rnf v


gradient :: Iso -> V3 Double -> V3 Double
gradient i v@(V3 x y z) = V3 dx dy dz
  where v0 = i v
        dx = (i (V3 (x + ε) y z) - v0) / ε
        dy = (i (V3 x (y + ε) z) - v0) / ε
        dz = (i (V3 x y (z + ε)) - v0) / ε


-- Start someplace with a negative iso value and seek the boundary by following
-- the gradient.
boundary_from :: Iso -> V3 Double -> V3 Double
boundary_from i v = go i 0 v
  where go i n v
          | n > 50     = v
          | abs v0 > ε = go i (n+1) (v ^-^ v0/2 *^ gradient i v)
          | otherwise  = v
          where v0 = i v


random_v :: IO (V3 Double)
random_v = do
  x <- r
  y <- r
  z <- r
  return $ V3 x y z
  where r = randomRIO (-1, 1)


iso_from_points :: Iso -> [V3 Double] -> Element
iso_from_points i ps = Multi (of_points b) $ pmap each b
  where each v = Shape (BB 0 1) identity [v, boundary_from i $ nv v]
        b      = map (boundary_from i) ps
        nv v   = v ^-^ ((gradient i v ^* 0.1) `cross` v)


align :: V3 Double -> V3 Double
align (V3 x y z)
  | ax > ay && ax > az = V3 x 0 0
  | ay > ax && ay > az = V3 0 y 0
  | otherwise          = V3 0 0 z
  where ax = abs x
        ay = abs y
        az = abs z


iso_crawl :: Iso -> Int -> Double -> V3 Double -> IO [V3 Double]
iso_crawl _ 0 _ _ = return []
iso_crawl i n d v = do
  rv <- cross (gradient i v) <$> random_v
  let v' = boundary_from i $ v ^+^ rv ^* d
  t <- iso_crawl i (n-1) d v'
  return $ v' : t


iso_crawler :: Int -> Double -> Iso -> IO Element
iso_crawler n d i = do
  setStdGen $ mkStdGen 0
  vs <- replicateM n random_v
  cs <- mapM (iso_crawl i n d) vs
  return $ Multi (BB (-2) 2) $ map (shape_of identity) cs


iso_element :: Int -> Iso -> IO Element
iso_element n i = do
  setStdGen $ mkStdGen 0
  iso_from_points i <$> replicateM n random_v


ifloat :: Int -> Int -> Double
ifloat i n = fromIntegral i / fromIntegral n


iso_scan :: Int -> Iso -> Element
iso_scan n i = iso_from_points i $
  flip map [1..n*n*n] \p ->
    let (xi, yi, zi) = unpack n p in
      V3 (ifloat xi n * 4 - 2) (ifloat yi n * 4 - 2) (ifloat zi n * 4 - 2)


pack :: Int -> (Int, Int, Int) -> Int
pack n (x, y, z) = n*(n*x + y) + z

unpack :: Int -> Int -> (Int, Int, Int)
unpack n xyz = (x, y, z)
  where (xy, z) = xyz `divMod` n
        (x, y)  = xy  `divMod` n


iso_scan2 :: Int -> Iso -> Element
iso_scan2 n i = Multi (BB (-2) 2) $ elems es
  where es = runSTArray do
          vs  <- newArray (0, n*n*n) (V3 0 0 0) :: ST s (STArray s Int (V3 Double))
          vis <- getAssocs vs
          forM_ vis \(p, _) -> do
            let (x, y, z) = unpack n p
            writeArray vs p $ boundary_from i $ V3 (ifloat x n * 4 - 2)
                                                   (ifloat y n * 4 - 2)
                                                   (ifloat z n * 4 - 2)

          ps  <- newArray_ (0, (n-1)*(n-1)*(n-1))
          pis <- getAssocs ps
          forM_ pis \(p, _) -> do
            let (x, y, z) = unpack (n-1) p
            va <- readArray vs $ pack n (x, y, z)
            v1 <- readArray vs $ pack n (x+1, y, z)
            v2 <- readArray vs $ pack n (x, y+1, z)
            v3 <- readArray vs $ pack n (x, y, z+1)

            let [_, vb, vc] = sortBy (compare `on` distance va) [v1, v2, v3]
            writeArray ps p $ shape_of identity [vb, va, vc]

          return ps
