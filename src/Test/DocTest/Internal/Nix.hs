{-# LANGUAGE LambdaCase #-}

module Test.DocTest.Internal.Nix where

import Control.Monad (msum)
import Control.Monad.Extra (ifM)
import Control.Monad.Trans.Maybe
import Data.Bool (bool)
import Data.List (intercalate, isSuffixOf)
import Data.Maybe (isJust)
import Data.Version
import GHC.Base (mzero)
import System.Directory
import System.Environment (lookupEnv)
import System.FilePath ((</>), isDrive, takeDirectory)
import System.Process (readProcess)

import GHC.Data.Maybe (liftMaybeT)
import System.Info (fullCompilerVersion)

-- | E.g. @9.0.2@
compilerVersionStr :: String
compilerVersionStr = intercalate "." (map show (versionBranch fullCompilerVersion))

-- | Traverse upwards until one of the following conditions is met:
--
--   * Current working directory is either root or a home directory
--   * The predicate function returns 'Just'
--
findDirectoryUp :: (FilePath -> IO (Maybe a)) -> MaybeT IO a
findDirectoryUp f = do
  home <- liftMaybeT getHomeDirectory
  MaybeT (go home =<< getCurrentDirectory)
 where
  go home cwd
    | isDrive cwd = pure Nothing
    | cwd == home = pure Nothing
    | otherwise =
      f cwd >>= \case
        Just a -> pure (Just a)
        Nothing -> go home (takeDirectory cwd)

-- | Like 'findDirectoryUp', but takes a predicate function instead. If the predicate
-- yields 'True', the filepath is returned.
findDirectoryUpPredicate :: (FilePath -> IO Bool) -> MaybeT IO FilePath
findDirectoryUpPredicate f = findDirectoryUp (\fp -> bool Nothing (Just fp) <$> f fp)

-- | Find the root of the Cabal project relative to the current directory.
findCabalProjectRoot :: MaybeT IO FilePath
findCabalProjectRoot =
  msum
    [ findDirectoryUpPredicate containsCabalProject
    , findDirectoryUpPredicate containsCabalPackage
    ]
 where
  containsCabalPackage :: FilePath -> IO Bool
  containsCabalPackage fp = elem "cabal.project" <$> getDirectoryContents fp

  containsCabalProject :: FilePath -> IO Bool
  containsCabalProject fp = any (".cabal" `isSuffixOf`) <$> getDirectoryContents fp

-- | Find the local package database in @dist-newstyle@.
findLocalPackageDb :: MaybeT IO FilePath
findLocalPackageDb = do
  projectRoot <- findCabalProjectRoot
  let
    relDir = "dist-newstyle" </> "packagedb" </> ("ghc-" ++ compilerVersionStr)
    absDir = projectRoot </> relDir
  ifM
    (liftMaybeT (doesDirectoryExist absDir))
    (return absDir)
    mzero

-- | Are we running in a Nix shell?
inNixShell :: IO Bool
inNixShell = isJust <$> lookupEnv "IN_NIX_SHELL"

-- | Are we running in a Nix build environment?
inNixBuild :: IO Bool
inNixBuild = isJust <$> lookupEnv "NIX_BUILD_TOP"

getLocalCabalPackageDbArgs :: IO [String]
getLocalCabalPackageDbArgs = do
  runMaybeT findLocalPackageDb >>= \case
     Nothing -> pure []
     Just s -> pure ["-package-db", s]

getLocalNixPackageDbArgs :: IO [String]
getLocalNixPackageDbArgs = do
  pkgDb <- makeAbsolute ("dist" </> "package.conf.inplace")
  ifM
    (doesDirectoryExist pkgDb)
    (pure ["-package-db", pkgDb])
    (pure [])

-- | Get global package db; used in a NIX_SHELL context
getGlobalPackageDb :: IO String
getGlobalPackageDb = init <$> readProcess "ghc" ["--print-global-package-db"] ""

-- | Get flags to be used when running in a Nix context (either in a build, or a
-- shell).
getNixGhciArgs :: IO [String]
getNixGhciArgs =
  ifM inNixShell goShell (ifM inNixBuild goBuild (pure []))
 where
  goShell = do
    globalPkgDb <- getGlobalPackageDb
    localPkgDbFlag <- getLocalCabalPackageDbArgs
    let globalDbFlag = ["-package-db", globalPkgDb]
    pure (defaultArgs ++ globalDbFlag ++ localPkgDbFlag)

  goBuild = do
    localDbFlag <- getLocalNixPackageDbArgs
    pure (defaultArgs ++ localDbFlag)

  defaultArgs =
    [ "-package-env", "-"

    -- Nix doesn't always expose the GHC library (_specifically_ the GHC lib) even
    -- if a package lists it as a dependency. This simply always exposes it as a
    -- workaround.
    , "-package", "ghc"
    ]
