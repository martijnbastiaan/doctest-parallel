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
  , "  doctest [ --fast | --preserve-it | --verbose | -jN ]..."
  , "  doctest --help"
  , "  doctest --version"
  , "  doctest --info"
  , ""
  , "Options:"
  , "  -jN                      number of threads to use"
  , "  --preserve-it            preserve the `it` variable between examples"
  , "  --verbose                print each test as it is run"
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
  -- ^ Module names to test
  , cfgThreads :: Maybe Int
  -- ^ Number of threads to use. Defaults to autodetection based on the number
  -- of cores.
  } deriving (Show, Eq)

defaultConfig :: Config
defaultConfig = Config
  { cfgPreserveIt = False
  , cfgVerbose = False
  , cfgModules = []
  , cfgThreads = Nothing
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
      "--preserve-it" -> go config{cfgPreserveIt=True} args
      "--verbose" -> go config{cfgVerbose=True} args
      ('-':'j':n0) | Just n1 <- parseThreads n0 -> go config{cfgThreads=Just n1} args
      ('-':_) -> ResultStderr ("Unknown command line argument: " <> arg)
      mod_ -> go config{cfgModules=mod_ : cfgModules config} args

parseThreads :: String -> Maybe Int
parseThreads n0 = do
  n1 <- readMaybe n0
  if n1 > 0 then Just n1 else Nothing

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
    (flag, ('=':opt)) -> (flag, Just opt)
    (flag, _) -> (flag, Nothing)
