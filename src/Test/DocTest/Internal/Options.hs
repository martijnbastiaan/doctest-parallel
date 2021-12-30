{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}

module Test.DocTest.Internal.Options where

import           Prelude ()
import           Prelude.Compat

import           Data.List.Compat

import qualified Paths_doctest_parallel
import           Data.Version (showVersion)

#if __GLASGOW_HASKELL__ < 900
import           Config as GHC
#else
import           GHC.Settings.Config as GHC
#endif

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
  , "  -jN                      number of threads to use"
  , "  --randomize-order        randomize order in which tests are run"
  , "  --seed                   use a specific seed to randomize test order"
  , "  --preserve-it            preserve the `it` variable between examples"
  , "  --verbose                print each test as it is run"
  , "  --quiet                  only print errors"
  , "  --help                   display this help and exit"
  , "  --version                output version information and exit"
  , "  --info                   output machine-readable version information and exit"
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
  { cfgPreserveIt :: Bool
  -- ^ Preserve the @it@ variable between examples (default: @False@)
  , cfgVerbose :: Bool
  -- ^ Verbose output (default: @False@)
  , cfgModules :: [ModuleName]
  -- ^ Module names to test. An empty list means "test all modules".
  , cfgThreads :: Maybe Int
  -- ^ Number of threads to use. Defaults to autodetection based on the number
  -- of cores.
  , cfgRandomizeOrder :: Bool
  -- ^ Randomize the order in which test cases in a module are run (default: @False@)
  , cfgSeed :: Maybe Int
  -- ^ Initialize random number generator used to randomize test cases when
  -- 'cfgRandomizeOrder' is set. If set to 'Nothing', a random seed is picked
  -- from a system RNG source on startup.
  , cfgQuiet :: Bool
  -- ^ Only print error messages, no status or progress messages (default: @False@)
  } deriving (Show, Eq)

defaultConfig :: Config
defaultConfig = Config
  { cfgPreserveIt = False
  , cfgVerbose = False
  , cfgModules = []
  , cfgThreads = Nothing
  , cfgRandomizeOrder = False
  , cfgSeed = Nothing
  , cfgQuiet = False
  }

parseOptions :: [String] -> Result Config
parseOptions = go defaultConfig
 where
  go config [] = Result config
  go config (arg:args) =
    case arg of
      "--help" -> ResultStdout usage
      "--info" -> ResultStdout info
      "--version" -> ResultStdout versionInfo
      "--randomize-order" -> go config{cfgRandomizeOrder=True} args
      "--preserve-it" -> go config{cfgPreserveIt=True} args
      "--verbose" -> go config{cfgVerbose=True} args
      "--quiet" -> go config{cfgQuiet=True} args
      ('-':_) | Just n <- parseSeed arg -> go config{cfgSeed=Just n} args
      ('-':_) | Just n <- parseThreads arg -> go config{cfgThreads=Just n} args
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
