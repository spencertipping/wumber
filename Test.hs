{-# LANGUAGE BlockArguments #-}

module Test where
import Control.Monad
import Control.Monad.Identity
import Control.Monad.RWS.Strict
import Cur
import Graphics.Gloss

square sz = replicateM_ 500 do
  ly sz
  rz 90.5
  rx 0.5

main = do square 1; return ()
