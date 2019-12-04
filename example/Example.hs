module Main where

import Control.Monad (forM_)
import System.Environment (getArgs, getProgName)

import WumberShell (wumber_main)

import qualified Iso as Iso


main :: IO ()
main = do
  argv <- getArgs
  case argv of [x] | ((_, e):_) <- filter ((== x) . fst) examples -> e
               _                                                  -> usage


examples :: [(String, IO ())]
examples = [("iso",    iso_example),
            ("say_hi", say_hi)]


say_hi = putStrLn "hi!"


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


iso_example = wumber_main Iso.example
