module Main where

import Control.Monad      (forM_)
import Linear.V3          (V3)
import System.Environment (getArgs, getProgName)

import Wumber
import WumberShell (wumber_main, wumber_live, type_is)

import qualified Iso as Iso


main :: IO ()
main = do
  argv <- getArgs
  case argv of [x] | ((_, e):_) <- filter ((== x) . fst) examples -> e
               _                                                  -> usage


examples :: [(String, IO ())]
examples = [("iso",      iso_example),
            ("iso-live", iso_live),
            ("say-hi",   putStrLn "hi!")]

  where iso_example = wumber_main Iso.example
        iso_live    = wumber_live "example" "Iso.hs" "example"
                                  (type_is :: FRep V3 ())


usage :: IO ()
usage = do
  p <- getProgName
  putStrLn ""
  putStrLn $ concat ["usage: ", p, " <example_name>"]
  putStrLn "(or, if running from stack: stack run -- <example_name>)"
  putStrLn ""
  putStrLn "where <example_name> is one of:"
  forM_ (map fst examples) (putStrLn . ("- " ++))
  putStrLn ""