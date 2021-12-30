module Main where

import Test.DocTest (mainFromCabal)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  mainFromCabal "doctest-parallel" ("--randomize-order":args)
