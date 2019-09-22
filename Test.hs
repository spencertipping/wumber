module Test where
import Graphics.Gloss

pic :: Picture
pic = color white $ pictures $ map (\x -> translate x x $ Circle x) [4..100]
