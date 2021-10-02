{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}

module Test.DocTest where

import           Prelude ()
import           Prelude.Compat

import qualified Data.Set as Set

import           Control.Monad (unless)
import           System.Exit (exitFailure)
import           System.IO

import qualified Control.Exception as E


import           Panic

import           Parse
import           Options
import           Runner

-- Cabal
import Distribution.Simple
  ( KnownExtension(ImplicitPrelude), Extension (DisableExtension) )

-- me
import Test.DocTest.Helpers
  ( Library (libDefaultExtensions), extractCabalLibrary, findCabalPackage
  , libraryToGhciArgs )

-- | Run doctest with given list of arguments.
--
-- Example:
--
-- doctest "clash-prelude" =<< getArgs
--
mainFromCabal :: String -> [String] -> IO ()
mainFromCabal libName cmdArgs = do
  lib <- extractCabalLibrary =<< findCabalPackage libName
  mainFromLibrary lib cmdArgs

mainFromLibrary :: Library -> [String] -> IO ()
mainFromLibrary lib (parseOptions -> (mods, opts)) =
  case opts of
    Output s -> putStr s
    Result (warnings, config) -> do
      mapM_ (hPutStrLn stderr) warnings
      hFlush stderr

      r <- main lib mods config `E.catch` \e -> do
        case fromException e of
          Just (UsageError err) -> do
            hPutStrLn stderr ("doctest: " ++ err)
            hPutStrLn stderr "Try `doctest --help' for more information."
            exitFailure
          _ -> E.throwIO e
      unless (isSuccess r) exitFailure

isSuccess :: Summary -> Bool
isSuccess s = sErrors s == 0 && sFailures s == 0

-- | Filter modules to be tested against a list of modules to be tested (specified
-- by the user on the command line). If list is empty, test all modules. Throws
-- and error if a non-existing module was specified.
filterModules :: [ModuleName] -> [Module a] -> [Module a]
filterModules [] mods = mods
filterModules wantedMods0 allMods0
  | (_:_) <- nonExistingMods = error ("Unknown modules specified: " <> show nonExistingMods)
  | otherwise = filter isSpecifiedMod allMods0
 where
  wantedMods1 = Set.fromList wantedMods0
  allMods1 = Set.fromList (map moduleName allMods0)

  nonExistingMods = Set.toList (wantedMods1 `Set.difference` allMods1)
  isSpecifiedMod Module{moduleName} = moduleName `Set.member` wantedMods1


main :: Library -> [ModuleName] -> Config -> IO Summary
main lib mods Config{..} = do
  let
    implicitPrelude = DisableExtension ImplicitPrelude `notElem` libDefaultExtensions lib
    (includeArgs, moduleArgs, otherGhciArgs) = libraryToGhciArgs lib
    evalGhciArgs = otherGhciArgs ++ ["-XNoImplicitPrelude"]

  -- get examples from Haddock comments
  allModules <- getDocTests (includeArgs ++ moduleArgs ++ otherGhciArgs)
  let modules = filterModules mods allModules
  runModules cfgPreserveIt cfgVerbose implicitPrelude evalGhciArgs modules
