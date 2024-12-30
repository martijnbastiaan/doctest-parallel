{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}

module MainSpec where

import           Test.Hspec
import           Test.HUnit (assertEqual, Assertion)

import qualified Data.Map as Map
import qualified Test.DocTest as DocTest
import           Test.DocTest.Helpers (extractSpecificCabalLibrary, findCabalPackage)
import           Test.DocTest.Internal.Options
import           Test.DocTest.Internal.Runner
import           System.Environment (getEnvironment)
import           System.IO.Silently
import           System.IO

-- | Construct a doctest specific 'Assertion'.
doctest :: HasCallStack => [ModuleName] -> Summary -> Assertion
doctest = doctestWithOpts defaultConfig

doctestWithOpts :: HasCallStack => Config -> [ModuleName] -> Summary -> Assertion
doctestWithOpts config modNames expected = do
  pkg <- findCabalPackage "doctest-parallel"
  lib <- extractSpecificCabalLibrary (Just "spectests-modules") pkg
  actual <-
    hSilence [stderr] $
      DocTest.run lib config{cfgModules=modNames}
  assertEqual (show modNames) expected actual

cases :: Int -> Summary
cases n = Summary n n 0 0

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  env <- Map.fromList <$> runIO getEnvironment

  let
    cDescribe =
      if
        -- Don't run doctests as part of the Stack testsuite yet, pending
        -- https://github.com/commercialhaskell/stack/issues/5662
        | "STACK_EXE" `Map.member` env -> xdescribe

        -- Don't run doctests as part of a Nix build. Similar to Stack, Nix
        -- doesn't seem to deal with private libraries yet.
        | "NIX_BUILD_TOP" `Map.member` env -> xdescribe

        | otherwise -> describe

  cDescribe "doctest" $ do
    it "testSimple" $
      doctest ["TestSimple.Fib"]
        (cases 1)

    it "it-variable" $ do
      doctestWithOpts (defaultConfig{cfgModuleConfig=defaultModuleConfig{cfgPreserveIt=True}}) ["It.Foo"]
        (cases 5)

    it "it-variable in $setup" $ do
      doctestWithOpts (defaultConfig{cfgModuleConfig=defaultModuleConfig{cfgPreserveIt=True}}) ["It.Setup"]
        (cases 2)

    it "failing" $ do
      doctest ["Failing.Foo"]
        (cases 1) {sFailures = 1}

    it "skips subsequent examples from the same group if an example fails" $
      doctest ["FailingMultiple.Foo"]
        (cases 4) {sTried = 2, sFailures = 1}

    it "use -DFIB=fib to set CPP flag" $
      doctestWithOpts defaultConfig{cfgGhcArgs=["-DFIB=fib"]} ["GhcArg.Fib"]
        (cases 1)

    it "testImport" $ do
      doctest ["TestImport.ModuleA"]
        (cases 2)

    it "testCommentLocation" $ do
      doctest ["TestCommentLocation.Foo"]
        (cases 11)

    it "testPutStr" $ do
      doctest ["TestPutStr.Fib"]
        (cases 3)

    it "fails on multi-line expressions, introduced with :{" $ do
      doctest ["TestFailOnMultiline.Fib"]
        (cases 2) {sErrors = 2}

    it "testBlankline" $ do
      doctest ["TestBlankline.Fib"]
        (cases 1)

    it "examples from the same Haddock comment share the same scope" $ do
      doctest ["TestCombinedExample.Fib"]
        (cases 4)

    it "testDocumentationForArguments" $ do
      doctest ["TestDocumentationForArguments.Fib"]
        (cases 1)

    it "template-haskell" $ do
      doctest ["TemplateHaskell.Foo"]
        (cases 2)

    it "handles source files with CRLF line endings" $ do
      doctest ["DosLineEndings.Fib"]
        (cases 1)

    it "runs $setup before each test group" $ do
      doctest ["Setup.Foo"]
        (cases 1)

    it "skips subsequent tests from a module, if $setup fails" $ do
      doctest ["SetupSkipOnFailure.Foo"]
        -- TODO: Introduce "skipped"
        (cases 2) {sTried = 0, sFailures = 1}

    it "works with additional object files" $ do
      doctest ["WithCbits.Bar"]
        (cases 1)

    it "ignores trailing whitespace when matching test output" $ do
      doctest ["TrailingWhitespace.Foo"]
        (cases 1)

  cDescribe "doctest as a runner for QuickCheck properties" $ do
    it "runs a boolean property" $ do
      doctest ["PropertyBool.Foo"]
        (cases 1)

    it "runs an explicitly quantified property" $ do
      doctest ["PropertyQuantified.Foo"]
        (cases 1)

    it "runs an implicitly quantified property" $ do
      doctest ["PropertyImplicitlyQuantified.Foo"]
        (cases 1)

    it "reports a failing property" $ do
      doctest ["PropertyFailing.Foo"]
        (cases 1) {sFailures = 1}

    it "runs a boolean property with an explicit type signature" $ do
      doctest ["PropertyBoolWithTypeSignature.Foo"]
        (cases 1)

    it "runs $setup before each property" $ do
      doctest ["PropertySetup.Foo"]
        (cases 1)

  cDescribe "doctest (module isolation)" $ do
    it "should fail due to module isolation" $ do
      doctestWithOpts defaultConfig ["ModuleIsolation.TestA", "ModuleIsolation.TestB"]
        (cases 2) {sFailures = 1}

  cDescribe "doctest (regression tests)" $ do
    it "bugfixOutputToStdErr" $ do
      doctest ["BugfixOutputToStdErr.Fib"]
        (cases 2)

    it "bugfixImportHierarchical" $ do
      doctest ["BugfixImportHierarchical.ModuleA", "BugfixImportHierarchical.ModuleB"]
        (cases 4)

    it "bugfixMultipleModules" $ do
      doctest ["BugfixMultipleModules.ModuleA", "BugfixMultipleModules.ModuleB"]
        -- TODO: Introduce "skipped"
        (cases 6) {sTried = 5, sFailures = 1}

    it "doesn't clash with user bindings of stdout/stderr" $ do
      doctest ["LocalStderrBinding.A"]
        (cases 1)

    it "doesn't get confused by doctests using System.IO imports" $ do
      doctest ["SystemIoImported.A"]
        (cases 2)

    it "correctly handles C import directories" $ do
      doctest ["WithCInclude.Bar"]
        (cases 1)

    it "sets module level options" $ do
      doctest ["ModuleOptions.Foo"]
        (cases 5)

    it "succeeds for non-exposed modules if --no-implicit-module-import is set" $ do
      doctest ["NonExposedModule.NoImplicitImport"]
        (cases 2)
