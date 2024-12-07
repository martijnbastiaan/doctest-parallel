{-# LANGUAGE MultiWayIf #-}

module ProjectsSpec (main, spec) where

import Test.Hspec
import System.Environment (getEnvironment)
import System.Process (readCreateProcess, proc)

import qualified Data.Map as Map

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  env <- Map.fromList <$> runIO getEnvironment

  let
    -- Only test with cabal
    cDescribe =
      if
        | "STACK_EXE" `Map.member` env -> xdescribe
        | "NIX_BUILD_TOP" `Map.member` env -> xdescribe
        | otherwise -> describe

  cDescribe "T85-default-language" $ do
    it "cabal run doctests" $ do
      _ <- readCreateProcess (proc "cabal" ["run", "-v0", "--", "doctests", "--quiet"]) ""
      pure ()
