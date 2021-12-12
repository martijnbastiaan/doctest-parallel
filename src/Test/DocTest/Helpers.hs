{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Test.DocTest.Helpers where

import GHC.Stack (HasCallStack)

import System.Directory
  ( canonicalizePath, doesFileExist )
import System.FilePath ((</>), isDrive, takeDirectory)
import System.FilePath.Glob (glob)

#if __GLASGOW_HASKELL__ < 804
import Data.Monoid ((<>))
#endif

-- Cabal
import Distribution.ModuleName (ModuleName)
import Distribution.Simple
  ( Extension (DisableExtension, EnableExtension, UnknownExtension) )
import Distribution.Types.UnqualComponentName ( unUnqualComponentName )
import Distribution.PackageDescription
  ( CondTree(CondNode, condTreeData), GenericPackageDescription (condLibrary)
  , exposedModules, libBuildInfo, hsSourceDirs, defaultExtensions, package
  , packageDescription, condSubLibraries )
import Distribution.Pretty (prettyShow)
import Distribution.Verbosity (silent)

#if MIN_VERSION_Cabal(3,6,0)
import Distribution.Utils.Path (SourceDir, PackageDir, SymbolicPath)
#endif

-- cabal-install-parsers
import Distribution.PackageDescription.Parsec (readGenericPackageDescription)

data Library = Library
  { libSourceDirectories :: [FilePath]
  , libModules :: [ModuleName]
  , libDefaultExtensions :: [Extension]
  }
  deriving (Show)

-- | Convert a "Library" to arguments suitable to be passed to GHCi.
libraryToGhciArgs :: Library -> ([String], [String], [String])
libraryToGhciArgs Library{..} = (srcArgs, modArgs, extArgs)
 where
  srcArgs = map ("-i" <>) libSourceDirectories
  modArgs = map prettyShow libModules
  extArgs = map showExt libDefaultExtensions

  showExt = \case
    EnableExtension ext -> "-X" <> show ext
    DisableExtension ext -> "-XNo" <> show ext
    UnknownExtension ext -> "-X" <> ext

-- | Drop a number of elements from the end of the list.
--
-- > dropEnd 3 "hello"  == "he"
-- > dropEnd 5 "bye"    == ""
-- > dropEnd (-1) "bye" == "bye"
-- > \i xs -> dropEnd i xs `isPrefixOf` xs
-- > \i xs -> length (dropEnd i xs) == max 0 (length xs - max 0 i)
-- > \i -> take 3 (dropEnd 5 [i..]) == take 3 [i..]
dropEnd :: Int -> [a] -> [a]
dropEnd i xs
  | i <= 0 = xs
  | otherwise = f xs (drop i xs)
 where
   f (a:as) (_:bs) = a : f as bs
   f _ _ = []

-- Searches for a file called @package.cabal@, where @package@ is given as an
-- argument. It will look for it in the current directory. If it can't find it
-- there, it will traverse up until it finds the file or a file called
-- @cabal.project@. In case of the latter, it will traverse down recursively
-- until it encounters a @package.cabal@.
--
-- The returned path points to the @package.cabal@. Errors if it could not
-- find @package.cabal@ anywhere, or when it found multiple.
--
findCabalPackage :: HasCallStack => String -> IO FilePath
findCabalPackage packageName = goUp =<< canonicalizePath packageName
 where
  goUp :: FilePath -> IO FilePath
  goUp path
    | isDrive path = error ("Could not find '" <> packageFilename <> "'")
    | otherwise = do
      packageExists <- doesFileExist (path </> packageFilename)
      projectExists <- doesFileExist (path </> projectFilename)

      if | packageExists -> pure (path </> packageFilename)
         | projectExists -> goDown path
         | otherwise -> goUp (takeDirectory path)

  goDown :: FilePath -> IO FilePath
  goDown path = do
    candidates <- glob (path </> "**" </> packageFilename)
    case candidates of
      [] -> error ("Could not find " <> packageFilename <> " in project " <> path)
      (_:_:_) -> error ("Ambiguous packages in project " <> path <> ": " <> show candidates)
      [c] -> pure c

  packageFilename = packageName <> ".cabal"
  projectFilename = "cabal.project"

#if MIN_VERSION_Cabal(3,6,0)
compatPrettyShow :: SymbolicPath PackageDir SourceDir -> FilePath
compatPrettyShow = prettyShow
#else
compatPrettyShow :: FilePath -> FilePath
compatPrettyShow = id
#endif

-- Given a filepath to a @package.cabal@, parse it, and yield a "Library". Yields
-- the default Library if first argument is Nothing, otherwise it will look for
-- a specific sublibrary.
extractSpecificCabalLibrary :: Maybe String -> FilePath -> IO Library
extractSpecificCabalLibrary maybeLibName pkgPath = do
  pkg <- readGenericPackageDescription silent pkgPath
  case maybeLibName of
    Nothing ->
      case condLibrary pkg of
        Nothing ->
          let pkgDescription = package (packageDescription pkg) in
          error ("Could not find main library in: " <> show pkgDescription)
        Just lib ->
          go lib

    Just libName ->
      go (findSubLib pkg libName (condSubLibraries pkg))

 where
  findSubLib pkg targetLibName [] =
    let pkgDescription = package (packageDescription pkg) in
    error ("Could not find library " <> targetLibName <> " in " <> show pkgDescription)
  findSubLib pkg targetLibName ((libName, lib):libs)
    | unUnqualComponentName libName == targetLibName = lib
    | otherwise = findSubLib pkg targetLibName libs

  go CondNode{condTreeData=lib} =
    let
      buildInfo = libBuildInfo lib
      sourceDirs = hsSourceDirs buildInfo
      root = takeDirectory pkgPath
    in
      pure Library
        { libSourceDirectories = map ((root </>) . compatPrettyShow) sourceDirs
        , libModules = exposedModules lib
        , libDefaultExtensions = defaultExtensions buildInfo
        }


-- Given a filepath to a @package.cabal@, parse it, and yield a "Library". Returns
-- and error if no library was specified in the cabal package file.
extractCabalLibrary :: FilePath -> IO Library
extractCabalLibrary = extractSpecificCabalLibrary Nothing
