{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}

module Test.DocTest
  ( mainFromCabal
  , mainFromLibrary
  , mainFromCabalWithConfig
  , mainFromLibraryWithConfig

  -- * Internal
  , filterModules
  , isSuccess
  , getSeed
  , run
  ) where

import           Prelude ()
import           Prelude.Compat

import qualified Data.Set as Set

import           Control.Monad (unless)
import           System.Exit (exitFailure)
import           System.IO
import           System.Random (randomIO)

import qualified Control.Exception as E

#if __GLASGOW_HASKELL__ < 900
import Panic
#else
import GHC.Utils.Panic
#endif

import Test.DocTest.Internal.Parse
import Test.DocTest.Internal.Options
import Test.DocTest.Internal.Runner

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
-- @
-- mainFromCabal "my-project" =<< getArgs
-- @
--
mainFromCabal :: String -> [String] -> IO ()
mainFromCabal libName cmdArgs = do
  lib <- extractCabalLibrary =<< findCabalPackage libName
  mainFromLibrary lib cmdArgs

-- | Run doctest given config.
--
-- Example:
--
-- @
-- mainFromCabal "my-project" defaultConfig
-- @
--
mainFromCabalWithConfig :: String -> Config -> IO ()
mainFromCabalWithConfig libName config = do
  lib <- extractCabalLibrary =<< findCabalPackage libName
  mainFromLibraryWithConfig lib config

-- | Like 'mainFromCabal', but with a given library.
mainFromLibrary :: Library -> [String] -> IO ()
mainFromLibrary lib (parseOptions -> opts) =
  case opts of
    ResultStdout s -> putStr s
    ResultStderr s -> do
       hPutStrLn stderr ("doctest: " ++ s)
       hPutStrLn stderr "Try `doctest --help' for more information."
       exitFailure
    Result config -> do
      mainFromLibraryWithConfig lib config

-- | Run doctests with given library and config.
mainFromLibraryWithConfig :: Library -> Config -> IO ()
mainFromLibraryWithConfig lib config = do
  r <- run lib config `E.catch` \e -> do
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

getSeed :: Bool -> Maybe Int -> IO (Maybe Int)
getSeed False _ = pure Nothing
getSeed True (Just seed) = pure (Just seed)
getSeed True Nothing = do
  -- Using an abslute number to prevent copy+paste errors
  seed <- abs <$> randomIO
  putStrLn ("Using freshly generated seed to randomize test order: " <> show seed)
  pure (Just seed)

-- | Run doctest for given library and config. Produce a summary of all tests.
run :: Library -> Config -> IO Summary
run lib Config{..} = do
  let
    implicitPrelude = DisableExtension ImplicitPrelude `notElem` libDefaultExtensions lib
    (includeArgs, moduleArgs, otherGhciArgs) = libraryToGhciArgs lib
    evalGhciArgs = otherGhciArgs ++ ["-XNoImplicitPrelude"]

  seed <- getSeed cfgRandomizeOrder cfgSeed

  -- get examples from Haddock comments
  allModules <- getDocTests (includeArgs ++ moduleArgs ++ otherGhciArgs)
  runModules
    cfgThreads cfgPreserveIt cfgVerbose seed implicitPrelude evalGhciArgs
    (filterModules cfgModules allModules)
