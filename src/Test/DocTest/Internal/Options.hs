{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}

module Test.DocTest.Internal.Options where

import           Prelude ()
import           Prelude.Compat

import           Control.Monad.Trans.RWS (RWS, execRWS)
import qualified Control.Monad.Trans.RWS as RWS

import           Control.Monad (when)
import           Data.List.Compat

import qualified Paths_doctest_parallel
import           Data.Version (showVersion)

#if __GLASGOW_HASKELL__ < 900
import           Config as GHC
#else
import           GHC.Settings.Config as GHC
#endif

import           Test.DocTest.Internal.Interpreter (ghc)

usage :: String
usage = unlines [
    "Usage:"
  , "  doctest [ --fast | --preserve-it | --verbose ]..."
  , "  doctest --help"
  , "  doctest --version"
  , "  doctest --info"
  , ""
  , "Options:"
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
  | Result ([Warning], a)
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
  } deriving (Show, Eq)

defaultConfig :: Config
defaultConfig = Config
  { cfgPreserveIt = False
  , cfgVerbose = False
  , cfgModules = []
  }

isFlag :: String -> Bool
isFlag ('-':_) = True
isFlag _ = False

parseOptions :: [String] -> Result Config
parseOptions args
  | "--help" `elem` args    = ResultStdout usage
  | "--info" `elem` args    = ResultStdout info
  | "--version" `elem` args = ResultStdout versionInfo
  | otherwise =
      case execRWS parse () (defaultConfig, args) of
        ((config, extraArgs), warnings) ->
          case partition isFlag extraArgs of
            ([], mods) ->
              Result (warnings, config{cfgModules=mods})
            (unknownFlags, _mods) ->
              ResultStderr ("Unknown command line arguments: " <> show unknownFlags)
    where
      parse :: RWS () [Warning] (Config, [String]) ()
      parse = do
        stripPreserveIt
        stripVerbose

stripPreserveIt :: RWS () [Warning] (Config, [String]) ()
stripPreserveIt = stripFlag (\cfg -> cfg{cfgPreserveIt=True}) "--preserve-it"

stripVerbose :: RWS () [Warning] (Config, [String]) ()
stripVerbose = stripFlag (\cfg -> cfg{cfgVerbose=True}) "--verbose"

stripFlag :: (Config -> Config) -> String -> RWS () [Warning] (Config, [String]) ()
stripFlag setter flag = do
  (cfg, args) <- RWS.get
  when (flag `elem` args) $
    RWS.put (setter cfg, filter (/= flag) args)

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
