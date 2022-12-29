{-# LANGUAGE CPP #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module Test.DocTest.Internal.Runner where

import           Prelude hiding (putStr, putStrLn, error)

import           Control.Concurrent (Chan, writeChan, readChan, newChan, forkIO, ThreadId, myThreadId)
import           Control.Exception (SomeException, catch)
import           Control.Monad hiding (forM_)
import           Data.Foldable (forM_)
import           Data.Function (on)
import           Data.List (sortBy)
import           Data.Maybe (fromMaybe, maybeToList)
import           GHC.Conc (getNumProcessors)
import           System.IO (hPutStrLn, hPutStr, stderr, hIsTerminalDevice)
import           System.Random (randoms, mkStdGen)
import           Text.Printf (printf)

import           Control.Monad.Trans.State
import           Control.Monad.IO.Class

import           Test.DocTest.Internal.Interpreter (Interpreter)
import qualified Test.DocTest.Internal.Interpreter as Interpreter
import           Test.DocTest.Internal.Parse
import           Test.DocTest.Internal.Options
  ( ModuleName, ModuleConfig (cfgPreserveIt), cfgSeed, cfgPreserveIt
  , cfgRandomizeOrder, cfgImplicitModuleImport, parseLocatedModuleOptions)
import           Test.DocTest.Internal.Location
import qualified Test.DocTest.Internal.Property as Property
import           Test.DocTest.Internal.Runner.Example
import           Test.DocTest.Internal.Logging (LogLevel (..), formatLog, shouldLog)

import           System.IO.CodePage (withCP65001)
import Control.Monad.Extra (whenM)

#if __GLASGOW_HASKELL__ < 804
import Data.Semigroup
#endif

-- | Whether an "example" is part of setup block
data FromSetup = FromSetup | NotFromSetup

-- | Summary of a test run.
data Summary = Summary {
    sExamples :: Int  -- ^ Total number of lines of examples (excluding setup)
  , sTried    :: Int  -- ^ Executed /sTried/ lines so  far
  , sErrors   :: Int  -- ^ Couldn't execute /sErrors/ examples
  , sFailures :: Int  -- ^ Got unexpected output for /sFailures/ examples
} deriving Eq

emptySummary :: Summary
emptySummary = Summary 0 0 0 0

-- | Format a summary.
instance Show Summary where
  show (Summary examples tried errors failures) =
    printf "Examples: %d  Tried: %d  Errors: %d  Unexpected output: %d" examples tried errors failures


-- | Sum up summaries.
instance Monoid Summary where
  mempty = Summary 0 0 0 0
#if __GLASGOW_HASKELL__ < 804
  mappend = (<>)
#endif

instance Semigroup Summary where
  (<>) (Summary x1 x2 x3 x4) (Summary y1 y2 y3 y4) =
    Summary (x1 + y1) (x2 + y2) (x3 + y3) (x4 + y4)

-- | Run all examples from a list of modules.
runModules
  :: (?verbosity::LogLevel)
  => ModuleConfig
  -- ^ Configuration options specific to module
  -> Maybe Int
  -- ^ Number of threads to use. Defaults to 'getNumProcessors'.
  -> Bool
  -- ^ Implicit Prelude
  -> [String]
  -- ^ Arguments passed to the GHCi process.
  -> [Module [Located DocTest]]
  -- ^ Modules under test
  -> IO Summary
runModules modConfig nThreads implicitPrelude args modules = do
  isInteractive <- hIsTerminalDevice stderr

  -- Start a thread pool. It sends status updates to this thread through 'output'.
  nCores <- getNumProcessors
  (input, output) <-
    makeThreadPool
      (fromMaybe nCores nThreads)
      (runModule modConfig implicitPrelude args)

  -- Send instructions to threads
  liftIO (mapM_ (writeChan input) modules)

  let
    nExamples = (sum . map count) modules
    initState = ReportState
      { reportStateCount = 0
      , reportStateInteractive = isInteractive
      , reportStateSummary = mempty{sExamples=nExamples}
      }

  threadId <- myThreadId
  let ?threadId = threadId

  ReportState{reportStateSummary} <- (`execStateT` initState) $ do
    consumeUpdates output (length modules)
    gets (show . reportStateSummary) >>= report Info

  return reportStateSummary
 where
  consumeUpdates ::
    (?threadId :: ThreadId) =>
    Chan (ThreadId, ReportUpdate) ->
    Int ->
    StateT ReportState IO ()
  consumeUpdates _output 0 = pure ()
  consumeUpdates output modsLeft = do
    (threadId, update) <- liftIO (readChan output)
    let ?threadId = threadId
    consumeUpdates output =<<
      case update of
        UpdateInternalError fs loc e -> reportInternalError fs loc e >> pure (modsLeft - 1)
        UpdateImportError modName result -> reportImportError modName result >> pure (modsLeft - 1)
        UpdateSuccess fs -> reportSuccess fs >> reportProgress >> pure modsLeft
        UpdateFailure fs loc expr errs -> reportFailure fs loc expr errs >> pure modsLeft
        UpdateError fs loc expr err -> reportError fs loc expr err >> pure modsLeft
        UpdateOptionError loc err -> reportOptionError loc err >> pure modsLeft
        UpdateModuleDone -> pure (modsLeft - 1)
        UpdateLog lvl msg -> report lvl msg >> pure modsLeft

-- | Count number of expressions in given module.
count :: Module [Located DocTest] -> Int
count (Module _ _ tests _) = sum (map length tests)

-- | A monad for generating test reports.
type Report = StateT ReportState IO

data ReportState = ReportState {
  reportStateCount        :: Int     -- ^ characters on the current line
, reportStateInteractive  :: Bool    -- ^ should intermediate results be printed?
, reportStateSummary      :: Summary -- ^ test summary
}

-- | Add output to the report.
report ::
  ( ?verbosity :: LogLevel
  , ?threadId :: ThreadId
  ) =>
  LogLevel ->
  String ->
  Report ()
report lvl msg0 =
  when (shouldLog lvl) $ do
    let msg1 = formatLog ?threadId lvl msg0
    overwrite msg1

    -- add a newline, this makes the output permanent
    liftIO $ hPutStrLn stderr ""
    modify (\st -> st {reportStateCount = 0})

-- | Add intermediate output to the report.
--
-- This will be overwritten by subsequent calls to `report`/`report_`.
-- Intermediate out may not contain any newlines.
report_ :: (?verbosity :: LogLevel) => LogLevel -> String -> Report ()
report_ lvl msg =
  when (shouldLog lvl) $ do
    whenM (gets reportStateInteractive) $ do
      overwrite msg
      modify (\st -> st {reportStateCount = length msg})

-- | Add output to the report, overwrite any intermediate out.
overwrite :: String -> Report ()
overwrite msg = do
  n <- gets reportStateCount
  let str | 0 < n     = "\r" ++ msg ++ replicate (n - length msg) ' '
          | otherwise = msg
  liftIO (hPutStr stderr str)

-- | Shuffle a list given a seed for an RNG
shuffle :: Int -> [a] -> [a]
shuffle seed xs =
    map snd
  $ sortBy (compare `on` fst)
  $ zip (randoms @Int (mkStdGen seed)) xs

-- | Run all examples from given module.
runModule
  :: ModuleConfig
  -> Bool
  -> [String]
  -> Chan (ThreadId, ReportUpdate)
  -> Module [Located DocTest]
  -> IO ()
runModule modConfig0 implicitPrelude ghciArgs output mod_ = do
  threadId <- myThreadId
  let update r = writeChan output (threadId, r)

  case modConfig2 of
    Left (loc, flag) ->
      update (UpdateOptionError loc flag)

    Right modConfig3 -> do
      let
        examples1
          | cfgRandomizeOrder modConfig3 = shuffle seed examples0
          | otherwise = examples0

        importModule
          | cfgImplicitModuleImport modConfig3 = Just (":m +" ++ module_)
          | otherwise = Nothing

        preserveIt = cfgPreserveIt modConfig3
        seed = fromMaybe 0 (cfgSeed modConfig3) -- Should have been set already

        reload repl = do
          void $ Interpreter.safeEval repl ":reload"
          mapM_ (Interpreter.safeEval repl) $
            if implicitPrelude
            then ":m Prelude" : maybeToList importModule
            else maybeToList importModule

          when preserveIt $
            -- Evaluate a dumb expression to populate the 'it' variable NOTE: This is
            -- one reason why we cannot have safeEval = safeEvalIt: 'it' isn't set in
            -- a fresh GHCi session.
            void $ Interpreter.safeEval repl $ "()"

        setup_ repl = do
          reload repl
          forM_ setup $ \l -> forM_ l $ \(Located _ x) -> case x of
            Property _  -> return ()
            Example e _ -> void $ safeEvalWith preserveIt repl e


      let logger = update . UpdateLog Debug
      Interpreter.withInterpreter logger ghciArgs $ \repl -> withCP65001 $ do
        -- Try to import this module, if it fails, something is off
        importResult <-
          case importModule of
            Nothing -> pure (Right "")
            Just i -> Interpreter.safeEval repl i

        case importResult of
          Right "" -> do
            -- Run setup group
            successes <-
              mapM
                (runTestGroup FromSetup preserveIt repl (reload repl) update)
                setup

            -- only run tests, if setup does not produce any errors/failures
            when
              (and successes)
              (mapM_
                (runTestGroup NotFromSetup preserveIt repl (setup_ repl) update)
                examples1)
          _ ->
            update (UpdateImportError module_ importResult)

  -- Signal main thread a module has been tested
  update UpdateModuleDone

  pure ()

 where
  Module module_ setup examples0 modArgs = mod_
  modConfig2 = parseLocatedModuleOptions module_ modConfig0 modArgs

data ReportUpdate
  = UpdateSuccess FromSetup
  -- ^ Test succeeded
  | UpdateFailure FromSetup Location Expression [String]
  -- ^ Test failed with unexpected result
  | UpdateError FromSetup Location Expression String
  -- ^ Test failed with an error
  | UpdateModuleDone
  -- ^ All examples tested in module
  | UpdateInternalError FromSetup (Module [Located DocTest]) SomeException
  -- ^ Exception caught while executing internal code
  | UpdateImportError ModuleName (Either String String)
  -- ^ Could not import module
  | UpdateOptionError Location String
  -- ^ Unrecognized flag in module specific option
  | UpdateLog LogLevel String
  -- ^ Unstructured message

makeThreadPool ::
  Int ->
  (Chan (ThreadId, ReportUpdate) -> Module [Located DocTest] -> IO ()) ->
  IO (Chan (Module [Located DocTest]), Chan (ThreadId, ReportUpdate))
makeThreadPool nThreads mutator = do
  input <- newChan
  output <- newChan
  forM_ [1..nThreads] $ \_ ->
    forkIO $ forever $ do
      i <- readChan input
      threadId <- myThreadId
      catch
        (mutator output i)
        (\e -> writeChan output (threadId, UpdateInternalError NotFromSetup i e))
  return (input, output)

reportFailure :: (?verbosity::LogLevel, ?threadId::ThreadId) => FromSetup -> Location -> Expression -> [String] -> Report ()
reportFailure fromSetup loc expression err = do
  report Error (printf "%s: failure in expression `%s'" (show loc) expression)
  mapM_ (report Error) err
  report Error ""
  updateSummary fromSetup (Summary 0 1 0 1)

reportError :: (?verbosity::LogLevel, ?threadId::ThreadId) => FromSetup -> Location -> Expression -> String -> Report ()
reportError fromSetup loc expression err = do
  report Error (printf "%s: error in expression `%s'" (show loc) expression)
  report Error err
  report Error ""
  updateSummary fromSetup (Summary 0 1 1 0)

reportOptionError :: (?verbosity::LogLevel, ?threadId::ThreadId) => Location -> String -> Report ()
reportOptionError loc opt = do
  report Error (printf "%s: unrecognized option: %s. Try --help to see all options." (show loc) opt)
  report Error ""
  updateSummary FromSetup (Summary 0 1 1 0)

reportInternalError :: (?verbosity::LogLevel, ?threadId::ThreadId) => FromSetup -> Module a -> SomeException -> Report ()
reportInternalError fs mod_ err = do
  report Error (printf "Internal error when executing tests in %s" (moduleName mod_))
  report Error (show err)
  report Error ""
  updateSummary fs emptySummary{sErrors=1}

reportImportError :: (?verbosity::LogLevel, ?threadId::ThreadId) => ModuleName -> Either String String -> Report ()
reportImportError modName importResult = do
  report Error ("Could not import module: " <> modName <> ". This can be caused by a number of issues: ")
  report Error ""
  report Error " 1. A module found by GHC contained tests, but was not in 'exposed-modules'. If you want"
  report Error "    to test non-exposed modules follow the instructions here:"
  report Error "    https://github.com/martijnbastiaan/doctest-parallel#test-non-exposed-modules"
  report Error ""
  report Error " 2. For Cabal users: Cabal did not generate a GHC environment file. Either:"
  report Error "   * Run with '--write-ghc-environment-files=always'"
  report Error "   * Add 'write-ghc-environment-files: always' to your cabal.project"
  report Error ""
  report Error " 3. For Cabal users: Cabal did not generate a GHC environment file in time. This"
  report Error "    can happen if you use 'cabal test' instead of 'cabal run doctests'. See"
  report Error "    https://github.com/martijnbastiaan/doctest-parallel/issues/22."
  report Error ""
  report Error " 4. The testsuite executable does not have a dependency on your project library. Please"
  report Error "    add it to the 'build-depends' section of the testsuite executable."
  report Error ""
  report Error "See the example project at https://github.com/martijnbastiaan/doctest-parallel/blob/main/example/README.md for more information."
  report Error ""
  report Error "The original reason given by GHCi was:"
  report Error ""
  case importResult of
    Left out -> do
      report Error "Unexpected output:"
      report Error out
    Right err -> do
      report Error "Error:"
      report Error err

  updateSummary FromSetup emptySummary{sErrors=1}

reportSuccess :: (?verbosity::LogLevel, ?threadId::ThreadId) => FromSetup -> Report ()
reportSuccess fromSetup = updateSummary fromSetup (Summary 0 1 0 0)

updateSummary :: FromSetup -> Summary -> Report ()
updateSummary FromSetup summary =
  -- Suppress counts, except for errors and unexpected outputs
  updateSummary NotFromSetup summary{sExamples=0, sTried=0}
updateSummary NotFromSetup summary = do
  ReportState n f s <- get
  put (ReportState n f $ s `mappend` summary)

reportProgress :: (?verbosity::LogLevel) => Report ()
reportProgress = gets (show . reportStateSummary) >>= report_ Info

-- | Run given test group.
--
-- The interpreter state is zeroed with @:reload@ first.  This means that you
-- can reuse the same 'Interpreter' for several test groups.
runTestGroup ::
  FromSetup ->
  Bool ->
  Interpreter ->
  IO () ->
  (ReportUpdate -> IO ()) ->
  [Located DocTest] ->
  IO Bool
runTestGroup fromSetup preserveIt repl setup update tests = do
  setup
  successExamples <- runExampleGroup fromSetup preserveIt repl update examples

  successesProperties <- forM properties $ \(loc, expression) -> do
    r <- do
      setup
      update (UpdateLog Verbose ("Started property at " ++ show loc))
      Property.runProperty repl expression

    case r of
      Property.Success -> do
        update (UpdateSuccess fromSetup)
        pure True
      Property.Error err -> do
        update (UpdateError fromSetup loc expression err)
        pure False
      Property.Failure msg -> do
        update (UpdateFailure fromSetup loc expression [msg])
        pure False

  pure (successExamples && and successesProperties)
  where
    properties = [(loc, p) | Located loc (Property p) <- tests]

    examples :: [Located Interaction]
    examples = [Located loc (e, r) | Located loc (Example e r) <- tests]

-- |
-- Execute all expressions from given example in given 'Interpreter' and verify
-- the output.
runExampleGroup ::
  FromSetup ->
  Bool ->
  Interpreter ->
  (ReportUpdate -> IO ()) ->
  [Located Interaction] ->
  IO Bool
runExampleGroup fromSetup preserveIt repl update examples = do
  threadId <- myThreadId
  go threadId examples
  where
    go threadId ((Located loc (expression, expected)) : xs) = do
      update (UpdateLog Verbose ("Started example at " ++ show loc))
      r <- fmap lines <$> safeEvalWith preserveIt repl expression
      case r of
        Left err -> do
          update (UpdateError fromSetup loc expression err)
          pure False
        Right actual -> case mkResult expected actual of
          NotEqual err -> do
            update (UpdateFailure fromSetup loc expression err)
            pure False
          Equal -> do
            update (UpdateSuccess fromSetup)
            go threadId xs
    go _ [] =
      pure True

safeEvalWith :: Bool -> Interpreter -> String -> IO (Either String String)
safeEvalWith preserveIt
  | preserveIt = Interpreter.safeEvalIt
  | otherwise  = Interpreter.safeEval
