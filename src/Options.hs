{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
module Options (
  Result(..)
, Config(..)
, defaultConfig
, parseOptions
#ifdef TEST
, usage
, info
, versionInfo
#endif
) where

import           Prelude ()
import           Prelude.Compat

import           Control.Monad.Trans.RWS (RWS, execRWS)
import qualified Control.Monad.Trans.RWS as RWS

import           Control.Monad (when)
import           Data.List.Compat

import qualified Paths_doctest
import           Data.Version (showVersion)

#if __GLASGOW_HASKELL__ < 900
import           Config as GHC
#else
import           GHC.Settings.Config as GHC
#endif

import           Interpreter (ghc)

usage :: String
usage = unlines [
    "Usage:"
  , "  doctest [ --fast | --preserve-it | --no-magic | --verbose | --no-isolate-modules | GHC OPTION | MODULE ]..."
  , "  doctest --help"
  , "  doctest --version"
  , "  doctest --info"
  , ""
  , "Options:"
  , "  --fast               disable :reload between example groups"
  , "  --preserve-it        preserve the `it` variable between examples"
  , "  --verbose            print each test as it is run"
  , "  --no-isolate-modules disable module isolation; run all tests in single GHCi session"
  , "  --help               display this help and exit"
  , "  --version            output version information and exit"
  , "  --info               output machine-readable version information and exit"
  ]

version :: String
version = showVersion Paths_doctest.version

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

data Result a = Output String | Result ([Warning], a)
  deriving (Eq, Show, Functor)

type Warning = String

data Config = Config
  { cfgOptions :: [String]
  -- ^ Options passed to GHCi session (default: @[]@)
  , cfgMagicMode :: Bool
  -- ^ Try to automatically find package databases (default: @True@)
  , cfgFastMode :: Bool
  -- ^ Don't run @:reload@ between tests (default: @False@)
  , cfgPreserveIt :: Bool
  -- ^ Preserve the @it@ variable between examples (default: @False@)
  , cfgVerbose :: Bool
  -- ^ Verbose output (default: @False@)
  , cfgIsolateModules :: Bool
  -- ^ Run each module in a separate GHCi session (default: @True@)
  } deriving (Show, Eq)

defaultConfig :: Config
defaultConfig = Config
  { cfgOptions = []
  , cfgMagicMode = True
  , cfgFastMode = False
  , cfgPreserveIt = False
  , cfgVerbose = False
  , cfgIsolateModules = True
  }

parseOptions :: [String] -> Result Config
parseOptions args
  | "--help" `elem` args = Output usage
  | "--info" `elem` args = Output info
  | "--version" `elem` args = Output versionInfo
  | otherwise = case execRWS parse () (defaultConfig, args) of
      ((config, ghciArgs), warnings) ->
        Result (warnings, config{cfgOptions=ghciArgs})
    where
      parse :: RWS () [Warning] (Config, [String]) ()
      parse = do
        stripNoMagic
        stripFast
        stripPreserveIt
        stripVerbose
        stripNoIsolateModules

        -- must be executed last
        stripOptGhc

stripNoIsolateModules :: RWS () [Warning] (Config, [String]) ()
stripNoIsolateModules =
  stripFlag (\cfg -> cfg{cfgIsolateModules=False}) "--no-isolate-modules"

stripNoMagic :: RWS () [Warning] (Config, [String]) ()
stripNoMagic = stripFlag (\cfg -> cfg{cfgMagicMode=False}) "--no-magic"

stripFast :: RWS () [Warning] (Config, [String]) ()
stripFast = stripFlag (\cfg -> cfg{cfgFastMode=True}) "--fast"

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


stripOptGhc :: RWS () [Warning] (Config, [String]) ()
stripOptGhc = do
  (cfg, args0) <- RWS.get
  case go args0 of
    (False, _) -> pure ()
    (True, args1) -> do
      RWS.tell [warning]
      RWS.put (cfg, args1)
  where
    warning = "WARNING: --optghc is deprecated, doctest now accepts arbitrary GHC options\ndirectly."

    bimap f g (a, b) = (f a, g b)

    go :: [String] -> (Bool, [String])
    go [] = (False, [])
    go ("--optghc":opt:rest) = bimap (True ||) (opt:) (go rest)
    go (arg:rest)
      | ("--optghc", Just val) <- parseFlag arg = bimap (True ||) (val:) (go rest)
      | otherwise = bimap (False ||) (arg:) (go rest)
