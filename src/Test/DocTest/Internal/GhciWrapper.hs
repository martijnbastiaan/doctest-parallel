{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}

module Test.DocTest.Internal.GhciWrapper (
  Interpreter
, Config(..)
, defaultConfig
, new
, close
, eval
, evalIt
, evalEcho
) where

import System.IO hiding (stdin, stdout, stderr)
import System.Process
import System.Exit
import Control.Monad
import Control.Exception
import Data.List
import Data.Maybe

import Test.DocTest.Internal.Logging (DebugLogger)

data Config = Config {
  configGhci :: String
, configVerbose :: Bool
, configIgnoreDotGhci :: Bool
} deriving (Eq, Show)

defaultConfig :: Config
defaultConfig = Config {
  configGhci = "ghci"
, configVerbose = False
, configIgnoreDotGhci = True
}

-- | Truly random marker, used to separate expressions.
--
-- IMPORTANT: This module relies upon the fact that this marker is unique.  It
-- has been obtained from random.org.  Do not expect this module to work
-- properly, if you reuse it for any purpose!
marker :: String
marker = show "dcbd2a1e20ae519a1c7714df2859f1890581d57fac96ba3f499412b2f5c928a1"

itMarker :: String
itMarker = "d42472243a0e6fc481e7514cbc9eb08812ed48daa29ca815844d86010b1d113a"

data Interpreter = Interpreter {
    hIn  :: Handle
  , hOut :: Handle
  , process :: ProcessHandle
  , logger :: DebugLogger
  }

new :: DebugLogger -> Config -> [String] -> IO Interpreter
new logger Config{..} args_ = do
  logger ("Calling: " ++ unwords (configGhci:args))
  (Just stdin_, Just stdout_, Nothing, processHandle ) <- createProcess $ (proc configGhci args) {std_in = CreatePipe, std_out = CreatePipe, std_err = Inherit}
  setMode stdin_
  setMode stdout_
  let
    interpreter = Interpreter
      { hIn = stdin_
      , hOut = stdout_
      , process = processHandle
      , logger=logger
      }
  _ <- eval interpreter "import qualified System.IO"
  _ <- eval interpreter "import qualified GHC.IO.Handle"
  -- The buffering of stdout and stderr is NoBuffering
  _ <- eval interpreter "GHC.IO.Handle.hDuplicateTo System.IO.stdout System.IO.stderr"
  -- Now the buffering of stderr is BlockBuffering Nothing
  -- In this situation, GHC 7.7 does not flush the buffer even when
  -- error happens.
  _ <- eval interpreter "GHC.IO.Handle.hSetBuffering System.IO.stdout GHC.IO.Handle.LineBuffering"
  _ <- eval interpreter "GHC.IO.Handle.hSetBuffering System.IO.stderr GHC.IO.Handle.LineBuffering"

  -- this is required on systems that don't use utf8 as default encoding (e.g.
  -- Windows)
  _ <- eval interpreter "GHC.IO.Handle.hSetEncoding System.IO.stdout System.IO.utf8"
  _ <- eval interpreter "GHC.IO.Handle.hSetEncoding System.IO.stderr System.IO.utf8"

  _ <- eval interpreter ":m - System.IO"
  _ <- eval interpreter ":m - GHC.IO.Handle"

  return interpreter
  where
    args = args_ ++ catMaybes [
        if configIgnoreDotGhci then Just "-ignore-dot-ghci" else Nothing
      , if configVerbose then Nothing else Just "-v0"
      ]
    setMode h = do
      hSetBinaryMode h False
      hSetBuffering h LineBuffering
      hSetEncoding h utf8

close :: Interpreter -> IO ()
close repl = do
  hClose $ hIn repl

  -- It is crucial not to close `hOut` before calling `waitForProcess`,
  -- otherwise ghci may not cleanly terminate on SIGINT (ctrl-c) and hang
  -- around consuming 100% CPU.  This happens when ghci tries to print
  -- something to stdout in its signal handler (e.g. when it is blocked in
  -- threadDelay it writes "Interrupted." on SIGINT).
  e <- waitForProcess $ process repl
  hClose $ hOut repl

  when (e /= ExitSuccess) $ do
    throwIO (userError $ "Test.DocTest.Internal.GhciWrapper.close: Interpreter exited with an error (" ++ show e ++ ")")

putExpression :: Interpreter -> Bool -> String -> IO ()
putExpression Interpreter{logger = logger, hIn = stdin} preserveIt e = do
  logger (">>> " ++ e)
  hPutStrLn stdin e

  when preserveIt $ do
    let e1 = "let " ++ itMarker ++ " = it"
    logger (">>> " ++ e1)
    hPutStrLn stdin e1

  hPutStrLn stdin (marker ++ " :: Data.String.String")

  when preserveIt $ do
    let e3 = "let it = " ++ itMarker
    logger (">>> " ++ e3)
    hPutStrLn stdin e3

  hFlush stdin

getResult :: Bool -> Interpreter -> IO String
getResult echoMode Interpreter{logger = logger, hOut = stdout} = do
  result <- go
  unless (result == mempty) $ logger result
  pure result
  where
    go = do
      line <- hGetLine stdout

      if
        | marker `isSuffixOf` line -> do
          let xs = stripMarker line
          echo xs
          return xs
        | otherwise -> do
          echo (line ++ "\n")
          result <- go
          return (line ++ "\n" ++ result)
    stripMarker l = take (length l - length marker) l

    echo :: String -> IO ()
    echo
      | echoMode = putStr
      | otherwise = (const $ return ())

-- | Evaluate an expression
eval :: Interpreter -> String -> IO String
eval repl expr = do
  putExpression repl False expr
  getResult False repl

-- | Like 'eval', but try to preserve the @it@ variable
evalIt :: Interpreter -> String -> IO String
evalIt repl expr = do
  putExpression repl True expr
  getResult False repl

-- | Evaluate an expression
evalEcho :: Interpreter -> String -> IO String
evalEcho repl expr = do
  putExpression repl False expr
  getResult True repl
