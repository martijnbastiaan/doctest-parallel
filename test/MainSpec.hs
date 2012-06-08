module MainSpec (main, spec) where

import           Test.Hspec.ShouldBe
import           Test.HUnit (assertEqual, Assertion)

import           Control.Exception
import           System.Directory (getCurrentDirectory, setCurrentDirectory)
import           System.FilePath
import           Report (Summary(..))
import           Run hiding (doctest)
import           System.IO.Silently
import           System.IO

withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory workingDir action = do
  bracket getCurrentDirectory setCurrentDirectory $ \_ -> do
    setCurrentDirectory workingDir
    action

-- | Construct a doctest specific 'Assertion'.
doctest :: FilePath   -- ^ current directory of `doctest` process
        -> [String]   -- ^ args, given to `doctest`
        -> Summary    -- ^ expected test result
        -> Assertion
doctest workingDir args summary = do
  r <- withCurrentDirectory ("test/integration" </> workingDir) (hSilence [stderr] $ doctest_ args)
  assertEqual label summary r
  where
    label = workingDir ++ " " ++ show args

cases :: Int -> Summary
cases n = Summary n n 0 0

main :: IO ()
main = hspecX spec

spec :: Specs
spec = do
  describe "doctest" $ do
    it "testSimple" $ do
      doctest "." ["testSimple/Fib.hs"]
        (cases 1)

    it "failing" $ do
      doctest "." ["failing/Foo.hs"]
        (cases 1) {sFailures = 1}

    it "testImport" $ do
      doctest "testImport" ["ModuleA.hs"]
        (cases 2)
      doctest ".." ["--optghc=-iintegration/testImport", "integration/testImport/ModuleA.hs"]
        (cases 2)

    it "testCommentLocation" $ do
      doctest "." ["testCommentLocation/Foo.hs"]
        (cases 10)

    it "testPutStr" $ do
      doctest "testPutStr" ["Fib.hs"]
        (cases 1)

    it "testFailOnMultiline" $ do
      doctest "testFailOnMultiline" ["Fib.hs"]
        (cases 1) {sErrors = 1}

    it "testBlankline" $ do
      doctest "testBlankline" ["Fib.hs"]
        (cases 1)

    it "testCombinedExample" $ do
      doctest "testCombinedExample" ["Fib.hs"]
        (cases 1)

    it "testDocumentationForArguments" $ do
      doctest "testDocumentationForArguments" ["Fib.hs"]
        (cases 1)

    it "template-haskell" $ do
      doctest "template-haskell" ["Foo.hs"]
        (cases 1)

    it "handles source files with CRLF line endings" $ do
      doctest "dos-line-endings" ["Fib.hs"]
        (cases 1)

  describe "doctest as a runner for QuickCheck properties" $ do
    it "runs a boolean property" $ do
      doctest "property-bool" ["Foo.hs"]
        (cases 1)

    it "runs an explicitly quantified property" $ do
      doctest "property-quantified" ["Foo.hs"]
        (cases 1)

    it "runs an implicitly quantified property" $ do
      doctest "property-implicitly-quantified" ["Foo.hs"]
        (cases 1)

    it "reports a failing property" $ do
      doctest "property-failing" ["Foo.hs"]
        (cases 1) {sFailures = 1}

    it "runs a boolean property with an explicit type signature" $ do
      doctest "property-bool-with-type-signature" ["Foo.hs"]
        (cases 1)

  describe "doctest (regression tests)" $ do
    it "bugfixWorkingDirectory" $ do
      doctest "bugfixWorkingDirectory" ["Fib.hs"]
        (cases 1)
      doctest "bugfixWorkingDirectory" ["examples/Fib.hs"]
        (cases 2)

    it "bugfixOutputToStdErr" $ do
      doctest "bugfixOutputToStdErr" ["Fib.hs"]
        (cases 1)

    it "bugfixMultipleStatements" $ do
      doctest "bugfixMultipleStatements" ["Fib.hs"]
        (cases 1)

    it "bugfixImportHierarchical" $ do
      doctest "bugfixImportHierarchical" ["ModuleA.hs", "ModuleB.hs"]
        (cases 2)

    it "bugfixMultipleModules" $ do
      doctest "bugfixMultipleModules" ["ModuleA.hs"]
        (cases 3)

    it "testCPP" $ do
      doctest "testCPP" ["--optghc=-cpp", "Foo.hs"]
        (cases 1) {sFailures = 1}
      doctest "testCPP" ["--optghc=-cpp", "--optghc=-DFOO", "Foo.hs"]
        (cases 1)
