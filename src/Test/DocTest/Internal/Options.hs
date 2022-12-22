{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module Test.DocTest.Internal.Options where

import           Prelude ()
import           Prelude.Compat

import           Control.DeepSeq (NFData)
import           Data.List.Compat
import           GHC.Generics (Generic)

import qualified Paths_doctest_parallel
import           Data.Version (showVersion)

#if __GLASGOW_HASKELL__ < 900
import           Config as GHC
#else
import           GHC.Settings.Config as GHC
#endif

import           Test.DocTest.Internal.Location (Located (Located), Location)
import           Test.DocTest.Internal.Interpreter (ghc)
import           Text.Read (readMaybe)

usage :: String
usage = unlines [
    "Usage:"
  , "  doctest [ options ]... [<module>]..."
  , "  doctest --help"
  , "  doctest --version"
  , "  doctest --info"
  , ""
  , "Options:"
  , "   -jN                      number of threads to use"
  , "†  --implicit-module-import import module before testing it (default)"
  , "†  --randomize-order        randomize order in which tests are run"
  , "†  --seed=N                 use a specific seed to randomize test order"
  , "†  --preserve-it            preserve the `it` variable between examples"
  , "   --nix                    account for Nix build environments (default)"
  , "   --verbose                print each test as it is run"
  , "   --quiet                  only print errors"
  , "   --help                   display this help and exit"
  , "   --version                output version information and exit"
  , "   --info                   output machine-readable version information and exit"
  , ""
  , "Supported inverted options:"
  , "   --no-nix"
  , "†  --no-implicit-module-import"
  , "†  --no-randomize-order (default)"
  , "†  --no-preserve-it (default)"
  , ""
  , "Options marked with a dagger (†) can also be used to set module level options, using"
  , "an ANN pragma like this:"
  , ""
  , "  {-# ANN module \"doctest-parallel: --no-randomize-order\" #-} "
  , ""
  ]

version :: String
version = showVersion Paths_doctest_parallel.version

ghcVersion :: String
ghcVersion = GHC.cProjectVersion

versionInfo :: String
versionInfo = unlines [
    "doctest version " ++ version
  , "using version " ++ ghcVersion ++ " of the GHC API"
  , "using " ++ ghc
  ]

info :: String
info = "[ " ++ (intercalate "\n, " . map show $ [
    ("version", version)
  , ("ghc_version", ghcVersion)
  , ("ghc", ghc)
  ]) ++ "\n]\n"

data Result a
  = ResultStderr String
  | ResultStdout String
  | Result a
  deriving (Eq, Show, Functor)

type Warning = String
type ModuleName = String

data Config = Config
  { cfgVerbose :: Bool
  -- ^ Verbose output (default: @False@)
  , cfgModules :: [ModuleName]
  -- ^ Module names to test. An empty list means "test all modules".
  , cfgThreads :: Maybe Int
  -- ^ Number of threads to use. Defaults to autodetection based on the number
  -- of cores.
  , cfgQuiet :: Bool
  -- ^ Only print error messages, no status or progress messages (default: @False@)
  , cfgModuleConfig :: ModuleConfig
  -- ^ Options specific to modules
  , cfgNix :: Bool
  -- ^ Detect Nix build environment and try to make GHC aware of the local package
  -- being tested.
  } deriving (Show, Eq, Generic, NFData)

data ModuleConfig = ModuleConfig
  { cfgPreserveIt :: Bool
  -- ^ Preserve the @it@ variable between examples (default: @False@)
  , cfgRandomizeOrder :: Bool
  -- ^ Randomize the order in which test cases in a module are run (default: @False@)
  , cfgSeed :: Maybe Int
  -- ^ Initialize random number generator used to randomize test cases when
  -- 'cfgRandomizeOrder' is set. If set to 'Nothing', a random seed is picked
  -- from a system RNG source on startup.
  , cfgImplicitModuleImport :: Bool
  -- ^ Import a module before testing it. Can be disabled to enabled to test
  -- non-exposed modules.
  } deriving (Show, Eq, Generic, NFData)

defaultModuleConfig :: ModuleConfig
defaultModuleConfig = ModuleConfig
  { cfgPreserveIt = False
  , cfgRandomizeOrder = False
  , cfgSeed = Nothing
  , cfgImplicitModuleImport = True
  }

defaultConfig :: Config
defaultConfig = Config
  { cfgVerbose = False
  , cfgModules = []
  , cfgThreads = Nothing
  , cfgQuiet = False
  , cfgModuleConfig = defaultModuleConfig
  , cfgNix = True
  }

parseLocatedModuleOptions ::
  ModuleName ->
  ModuleConfig ->
  [Located String] ->
  Either (Location, String) ModuleConfig
parseLocatedModuleOptions _modName modConfig [] = Right modConfig
parseLocatedModuleOptions modName modConfig0 (Located loc o:os) =
  case parseModuleOption modConfig0 o of
    Nothing ->
      Left (loc, o)
    Just modConfig1 ->
      parseLocatedModuleOptions modName modConfig1 os

parseModuleOption :: ModuleConfig -> String -> Maybe ModuleConfig
parseModuleOption config arg =
  case arg of
    "--randomize-order" -> Just config{cfgRandomizeOrder=True}
    "--no-randomize-order" -> Just config{cfgRandomizeOrder=False}
    "--preserve-it" -> Just config{cfgPreserveIt=True}
    "--no-preserve-it" -> Just config{cfgPreserveIt=False}
    "--implicit-module-import" -> Just config{cfgImplicitModuleImport=True}
    "--no-implicit-module-import" -> Just config{cfgImplicitModuleImport=False}
    ('-':_) | Just n <- parseSeed arg -> Just config{cfgSeed=Just n}
    _ -> Nothing

parseOptions :: [String] -> Result Config
parseOptions = go defaultConfig
 where
  go config [] = Result config
  go config (arg:args) =
    case arg of
      "--help" -> ResultStdout usage
      "--info" -> ResultStdout info
      "--version" -> ResultStdout versionInfo
      "--verbose" -> go config{cfgVerbose=True} args
      "--quiet" -> go config{cfgQuiet=True} args
      "--nix" -> go config{cfgNix=True} args
      "--no-nix" -> go config{cfgNix=False} args
      ('-':_) | Just n <- parseThreads arg -> go config{cfgThreads=Just n} args
      ('-':_)
        -- Module specific configuration options
        | Just modCfg <- parseModuleOption (cfgModuleConfig config) arg
       -> go config{cfgModuleConfig=modCfg} args
      ('-':_) -> ResultStderr ("Unknown command line argument: " <> arg)
      mod_ -> go config{cfgModules=mod_ : cfgModules config} args

-- | Parse seed argument
--
-- >>> parseSeed "--seed=6"
-- Just 6
-- >>> parseSeed "--seeeed=6"
-- Nothing
--
parseSeed :: String -> Maybe Int
parseSeed arg = readMaybe =<< parseSpecificFlag arg "seed"


-- | Parse number of threads argument
--
-- >>> parseThreads "-j6"
-- Just 6
-- >>> parseThreads "-j-2"
-- Nothing
-- >>> parseThreads "-jA"
-- Nothing
--
parseThreads :: String -> Maybe Int
parseThreads ('-':'j':n0) = do
  n1 <- readMaybe n0
  if n1 > 0 then Just n1 else Nothing
parseThreads _ = Nothing

-- | Parse a specific flag with a value, or return 'Nothing'
--
-- >>> parseSpecificFlag "--foo" "foo"
-- Nothing
-- >>> parseSpecificFlag "--foo=" "foo"
-- Nothing
-- >>> parseSpecificFlag "--foo=5" "foo"
-- Just "5"
-- >>> parseSpecificFlag "--foo=5" "bar"
-- Nothing
parseSpecificFlag :: String -> String -> Maybe String
parseSpecificFlag arg flag = do
  case parseFlag arg of
    ('-':'-':f, value) | f == flag -> value
    _ -> Nothing

-- | Parse a flag into its flag and argument component.
--
-- Example:
--
-- >>> parseFlag "--optghc=foo"
-- ("--optghc",Just "foo")
-- >>> parseFlag "--optghc="
-- ("--optghc",Nothing)
-- >>> parseFlag "--fast"
-- ("--fast",Nothing)
parseFlag :: String -> (String, Maybe String)
parseFlag arg =
  case break (== '=') arg of
    (flag, ['=']) -> (flag, Nothing)
    (flag, '=':opt) -> (flag, Just opt)
    (flag, _) -> (flag, Nothing)
