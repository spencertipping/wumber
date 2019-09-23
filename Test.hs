module Test where
import Graphics.Gloss
import Cur


for = flip map


loading :: Float -> Picture
loading t = color (makeColor 0.8 0.8 0.9 0.8)
  $ pictures
  $ for (map (* 10) [30..60])
  $ \i -> rotate (i * sin (t / 3) / 4) $ Circle i

pic :: Float -> Picture
pic = loading
