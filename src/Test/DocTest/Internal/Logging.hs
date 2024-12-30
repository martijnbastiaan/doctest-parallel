{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ViewPatterns #-}

module Test.DocTest.Internal.Logging where

import Control.Applicative (Alternative((<|>)))
import Control.Concurrent (ThreadId, myThreadId)
import Control.DeepSeq (NFData)
import Data.Char (toLower, toUpper)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)

#if MIN_VERSION_base(4,18,0)
import GHC.Conc.Sync (threadLabel)
#endif

#if !MIN_VERSION_base(4,18,0)
threadLabel :: ThreadId -> IO (Maybe String)
threadLabel _ = pure Nothing
#endif

-- | Convenience type alias - not used in this module, but sprinkled across the
-- project.
type DebugLogger = String -> IO ()

-- | Discards any log message
noLogger :: DebugLogger
noLogger = const (pure ())

data LogLevel
  = Debug
  -- ^ Intended for debug runs
  | Verbose
  -- ^ Intended for debug runs, but without flooding the user with internal messages
  | Info
  -- ^ Default log level - print messages user is likely wanting to see
  | Warning
  -- ^ Only print warnings
  | Error
  -- ^ Only print errors
  deriving (Show, Eq, Enum, Generic, NFData, Ord, Bounded)

-- | Case insensitive
--
-- >>> parseLogLevel "Info"
-- Just Info
-- >>> parseLogLevel "info"
-- Just Info
-- >>> parseLogLevel "errox"
-- Nothing
--
parseLogLevel :: String -> Maybe LogLevel
parseLogLevel (map toLower -> level) =
  foldl (<|>) Nothing (map go [minBound..])
 where
  go :: LogLevel -> Maybe LogLevel
  go l
    | map toLower (show l) == level = Just l
    | otherwise = Nothing

-- | Pretty print a 'LogLevel' in a justified manner, i.e., all outputs take the
-- same amount of characters to display.
--
-- >>> showJustifiedLogLevel Debug
-- "Debug  "
-- >>> showJustifiedLogLevel Verbose
-- "Verbose"
-- >>> showJustifiedLogLevel Info
-- "Info   "
-- >>> showJustifiedLogLevel Warning
-- "Warning"
-- >>> showJustifiedLogLevel Error
-- "Error  "
--
showJustifiedLogLevel :: LogLevel -> String
showJustifiedLogLevel = justifyLeft maxSizeLogLevel ' ' . show
 where
  maxSizeLogLevel :: Int
  maxSizeLogLevel = maximum (map (length . show) [(minBound :: LogLevel)..])

-- | Justify a list with a custom fill symbol
--
-- >>> justifyLeft 10 'x' "foo"
-- "fooxxxxxxx"
-- >>> justifyLeft 3 'x' "foo"
-- "foo"
-- >>> justifyLeft 2 'x' "foo"
-- "foo"
--
justifyLeft :: Int -> a -> [a] -> [a]
justifyLeft n c s = s ++ replicate (n - length s) c

-- | Pretty name for a 'ThreadId'. Uses 'threadLabel' if available, otherwise
-- falls back to 'show'.
getThreadName :: ThreadId -> IO String
getThreadName threadId = fromMaybe (show threadId) <$> threadLabel threadId

-- | /Prettily/ format a log message
--
-- > threadId <- myThreadId
-- > formatLog Debug (show threadId) "some debug message"
-- "[DEBUG  ] [ThreadId 1277462] some debug message"
--
formatLog :: String -> LogLevel -> String -> String
formatLog nm lvl msg =
  intercalate "\n" (map go (lines msg))
 where
  go = printf "[%s] [%s] %s" (map toUpper (showJustifiedLogLevel lvl)) nm

-- | Like 'formatLog', but instantiates the /thread/ argument with the current 'ThreadId'
--
-- > formatLogHere Debug "some debug message"
-- "[DEBUG  ] [ThreadId 1440849] some debug message"
--
formatLogHere :: LogLevel -> String -> IO String
formatLogHere lvl msg = do
  threadName <- getThreadName =<< myThreadId
  pure (formatLog threadName lvl msg)

-- | Should a message be printed? For a given verbosity level and message log level.
shouldLog :: (?verbosity :: LogLevel) => LogLevel -> Bool
shouldLog lvl = ?verbosity <= lvl

-- | Basic logging function. Uses 'formatLogHere'. Is not thread-safe.
log :: (?verbosity :: LogLevel) => LogLevel -> String -> IO ()
log lvl msg
  | shouldLog lvl = hPutStrLn stderr =<< formatLogHere lvl msg
  | otherwise = pure ()
