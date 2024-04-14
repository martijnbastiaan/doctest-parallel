{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ <= 906
{-# LANGUAGE LambdaCase #-}
#endif

module Test.DocTest.Internal.Interpreter (
  Interpreter
, safeEval
, safeEvalIt
, withInterpreter
, ghc
, interpreterSupported

-- * exported for testing
, ghcInfo
, haveInterpreterKey
) where

import System.Process
import System.Directory (getPermissions, executable)
import Control.Monad
import Control.Exception hiding (handle)
import Data.Char
#if __GLASGOW_HASKELL__ > 906
import Data.List (unsnoc)
#else
import Data.Bifunctor (first)
#endif
import GHC.Paths (ghc)

import Test.DocTest.Internal.GhciWrapper
import Test.DocTest.Internal.Logging (DebugLogger)

-- $setup
-- >>> import Test.DocTest.Internal.GhciWrapper (eval)
-- >>> import Test.DocTest.Internal.Logging (noLogger)

#if __GLASGOW_HASKELL__ <= 906
-- | If the list is empty returns 'Nothing', otherwise returns the 'init' and the 'last'.
--
-- > unsnoc "test" == Just ("tes",'t')
-- > unsnoc ""     == Nothing
-- > \xs -> unsnoc xs == if null xs then Nothing else Just (init xs, last xs)
unsnoc :: [a] -> Maybe ([a], a)
unsnoc = \case
  []   -> Nothing
  x:xs -> Just $ unsnoc1 x xs
  where
    unsnoc1 :: a -> [a] -> ([a], a)
    unsnoc1 x = \case
      []   -> ([], x)
      y:ys -> first (x:) $ unsnoc1 y ys
#endif

haveInterpreterKey :: String
haveInterpreterKey = "Have interpreter"

ghcInfo :: IO [(String, String)]
ghcInfo = read <$> readProcess ghc ["--info"] []

interpreterSupported :: IO Bool
interpreterSupported = do
  -- in a perfect world this permission check should never fail, but I know of
  -- at least one case where it did..
  x <- getPermissions ghc
  unless (executable x) $ do
    fail $ ghc ++ " is not executable!"

  maybe False (== "YES") . lookup haveInterpreterKey <$> ghcInfo

-- | Run an interpreter session.
--
-- Example:
--
-- >>> withInterpreter noLogger [] $ \i -> eval i "23 + 42"
-- "65\n"
withInterpreter
  :: DebugLogger            -- ^ Debug logger
  -> [String]               -- ^ List of flags, passed to GHC
  -> (Interpreter -> IO a)  -- ^ Action to run
  -> IO a                   -- ^ Result of action
withInterpreter logger flags action = do
  let
    args = flags ++ [
        "--interactive"
#if __GLASGOW_HASKELL__ >= 802
      , "-fdiagnostics-color=never"
      , "-fno-diagnostics-show-caret"
#endif
      ]
  bracket (new logger defaultConfig{configGhci = ghc} args) close action

-- | Evaluate an expression; return a Left value on exceptions.
--
-- An exception may e.g. be caused on unterminated multiline expressions.
safeEval :: Interpreter -> String -> IO (Either String String)
safeEval repl = either (return . Left) (fmap Right . eval repl) . filterExpression

safeEvalIt :: Interpreter -> String -> IO (Either String String)
safeEvalIt repl = either (return . Left) (fmap Right . evalIt repl) . filterExpression

filterExpression :: String -> Either String String
filterExpression e =
  case map strip (lines e) of
    [] -> Right e
    (firstLine:ls) ->
      let lastLine = maybe firstLine snd (unsnoc ls) in
      if firstLine == ":{" && lastLine /= ":}" then fail_ else Right e
  where
    fail_ = Left "unterminated multiline command"

    strip :: String -> String
    strip = dropWhile isSpace . reverse . dropWhile isSpace . reverse
