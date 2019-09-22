module Test where
import Graphics.Gloss
import Cur

pmap :: (a -> Picture) -> [a] -> Picture
pmap f xs = pictures $ map f xs

circles :: Float -> Picture
circles t = pmap (\x -> Circle ((sin t * x) `max` 20)) [30, 40, 100, 400, 500]

rects :: Float -> Picture
rects t = pmap (\x -> scale (cos t * x) (cos t * x)
                     $ rotate (x * x * t / 4)
                     $ rectangleWire x x) [1..30]

pic :: Float -> Picture
pic t = color white
  $ rotate (t * 10)
  $ pictures [translate (-400) 0 (circles t), translate 400 0 (rects t)]
