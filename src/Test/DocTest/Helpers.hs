{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Test.DocTest.Helpers where

import GHC.Stack (HasCallStack)

import System.Directory
  ( canonicalizePath, doesFileExist )
import System.FilePath ((</>), isDrive, takeDirectory)
import System.FilePath.Glob (glob)
import System.Info (compilerVersion)

#if __GLASGOW_HASKELL__ < 804
import Data.Monoid ((<>))
#endif

import qualified Data.Set as Set

-- Cabal
import Distribution.ModuleName (ModuleName)
import Distribution.Simple
  ( Extension (DisableExtension, EnableExtension, UnknownExtension) )
import Distribution.Types.UnqualComponentName ( unUnqualComponentName )
import Distribution.PackageDescription
  ( GenericPackageDescription (condLibrary)
  , exposedModules, libBuildInfo, hsSourceDirs, defaultExtensions, package
  , packageDescription, condSubLibraries, includeDirs, autogenModules
  , ConfVar(..), BuildInfo (cppOptions), hcOptions )

import Distribution.Compiler (CompilerFlavor(GHC))
import Distribution.Pretty (prettyShow)
import Distribution.System (buildArch, buildOS)
import Distribution.Types.Condition (Condition(..))
import Distribution.Types.CondTree
import Distribution.Types.Version (Version, mkVersion')
import Distribution.Types.VersionRange (withinRange)
import Distribution.Verbosity (silent)

#if MIN_VERSION_Cabal(3,8,0)
import Distribution.Simple.PackageDescription (readGenericPackageDescription)
#else
import Distribution.PackageDescription.Parsec (readGenericPackageDescription)
#endif

#if MIN_VERSION_Cabal(3,6,0)
import Distribution.Utils.Path (SourceDir, PackageDir, SymbolicPath)
#endif


-- | Efficient implementation of set like deletion on lists
--
-- >>> "abcd" `rmList` "ad"
-- "bc"
-- >>> "aaabcccd" `rmList` "ad"
-- "bccc"
rmList :: Ord a => [a] -> [a] -> [a]
rmList xs (Set.fromList -> ys) = filter (not . (`Set.member` ys)) xs

data Library = Library
  { libSourceDirectories :: [FilePath]
    -- ^ Haskell source directories
  , libCSourceDirectories :: [FilePath]
    -- ^ C source directories
  , libModules :: [ModuleName]
    -- ^ Exposed modules
  , libDefaultExtensions :: [Extension]
    -- ^ Extensions enabled by default
  , libFlags :: [String]
    -- ^ Flags found in 'cpp-options' and 'gcc-options'
  }
  deriving (Show)

-- | Merge multiple libraries into one, by concatenating all their fields.
mergeLibraries :: [Library] -> Library
mergeLibraries libs = Library
  { libSourceDirectories = concatMap libSourceDirectories libs
  , libCSourceDirectories = concatMap libCSourceDirectories libs
  , libModules = concatMap libModules libs
  , libDefaultExtensions = concatMap libDefaultExtensions libs
  , libFlags = concatMap libFlags libs
  }

-- | Convert a "Library" to arguments suitable to be passed to GHCi.
libraryToGhciArgs :: Library -> ([String], [String], [String])
libraryToGhciArgs Library{..} = (hsSrcArgs <> cSrcArgs, modArgs, extArgs)
 where
  hsSrcArgs = map ("-i" <>) libSourceDirectories
  cSrcArgs = map ("-I" <>) libCSourceDirectories
  modArgs = map prettyShow libModules
  extArgs = map showExt libDefaultExtensions <> libFlags

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

-- | Searches for a file called @package.cabal@, where @package@ is given as an
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

-- | Traverse the given tree, solve predicates in branches, and return its
-- contents.
--
-- XXX: Branches guarded by Cabal flags are ignored. I'm not sure where we should
--      get this info from.
--
solveCondTree :: CondTree ConfVar c a -> [(c, a)]
solveCondTree CondNode{condTreeData, condTreeConstraints, condTreeComponents} =
  (condTreeConstraints, condTreeData) : concatMap goBranch condTreeComponents
 where
  goBranch :: CondBranch ConfVar c a -> [(c, a)]
  goBranch (CondBranch condBranchCondition condBranchIfTrue condBranchIfFalse) =
    if   goCondition condBranchCondition
    then solveCondTree condBranchIfTrue
    else maybe mempty solveCondTree condBranchIfFalse

  goCondition :: Condition ConfVar -> Bool
  goCondition = \case
    Var cv ->
      case cv of
        OS os -> os == buildOS
        Arch ar -> ar == buildArch
        Impl cf versionRange ->
          case cf of
            GHC -> withinRange buildGhc versionRange
            _   -> error ("Unrecognized compiler: " <> show cf)
        -- XXX: We currently ignore any flags passed to Cabal
#if MIN_VERSION_Cabal(3,4,0)
        PackageFlag _fn -> False
#else
        Flag _fn -> False
#endif
    Lit b -> b
    CNot con -> not (goCondition con)
    COr con0 con1 -> goCondition con0 || goCondition con1
    CAnd con0 con1 -> goCondition con0 && goCondition con1

-- | GHC version as Cabal's 'Version' data structure
buildGhc :: Version
buildGhc = mkVersion' compilerVersion

-- | Given a filepath to a @package.cabal@, parse it, and yield a "Library". Yields
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
          pure (go lib)

    Just libName ->
      pure (go (findSubLib pkg libName (condSubLibraries pkg)))

 where
  findSubLib pkg targetLibName [] =
    let pkgDescription = package (packageDescription pkg) in
    error ("Could not find library " <> targetLibName <> " in " <> show pkgDescription)
  findSubLib pkg targetLibName ((libName, lib):libs)
    | unUnqualComponentName libName == targetLibName = lib
    | otherwise = findSubLib pkg targetLibName libs

  go condNode = mergeLibraries libs1
   where
    libs0 = map snd (solveCondTree condNode)
    libs1 = map goLib libs0

  goLib lib = Library
    { libSourceDirectories = map ((root </>) . compatPrettyShow) sourceDirs
    , libCSourceDirectories = map (root </>) cSourceDirs
    , libModules = exposedModules lib `rmList` autogenModules buildInfo
    , libDefaultExtensions = defaultExtensions buildInfo
    , libFlags = cppOptions buildInfo <> hcOptions GHC buildInfo
    }
   where
    buildInfo = libBuildInfo lib
    sourceDirs = hsSourceDirs buildInfo
    cSourceDirs = includeDirs buildInfo
    root = takeDirectory pkgPath


-- | Given a filepath to a @package.cabal@, parse it, and yield a "Library". Returns
-- and error if no library was specified in the cabal package file.
extractCabalLibrary :: FilePath -> IO Library
extractCabalLibrary = extractSpecificCabalLibrary Nothing
