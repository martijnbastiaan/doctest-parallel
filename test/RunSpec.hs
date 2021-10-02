{-# LANGUAGE CPP #-}
module RunSpec (main, spec) where

import           Prelude ()
import           Prelude.Compat

import           Test.Hspec
import           System.Exit

import qualified Control.Exception as E
import           Data.List.Compat

import           System.IO.Silently
import           System.IO (stderr)
import qualified Test.DocTest as DocTest
import           Test.DocTest.Helpers (findCabalPackage, extractSpecificCabalLibrary)
import qualified Test.DocTest.Internal.Options as Options

doctest :: HasCallStack => [String] -> IO ()
doctest args = do
  pkg <- findCabalPackage "doctest-parallel"
  lib <- extractSpecificCabalLibrary (Just "spectests-modules") pkg
  DocTest.mainFromLibrary lib args

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "doctest" $ do
    it "exits with ExitFailure if at least one test case fails" $ do
      hSilence [stderr] (doctest ["Failing.Foo"]) `shouldThrow` (== ExitFailure 1)

    it "prints help on --help" $ do
      (r, ()) <- capture (doctest ["--help"])
      r `shouldBe` Options.usage

    it "prints version on --version" $ do
      (r, ()) <- capture (doctest ["--version"])
      lines r `shouldSatisfy` any (isPrefixOf "doctest version ")

    it "prints error message on invalid option" $ do
      (r, e) <- hCapture [stderr] . E.try $ doctest ["--foo", "test/integration/test-options/Foo.hs"]
      e `shouldBe` Left (ExitFailure 1)
      r `shouldBe` unlines [
          "doctest: Unknown command line arguments: [\"--foo\"]"
        , "Try `doctest --help' for more information."
        ]

    -- The commented tests fail, but only because `doctest-parallel` prints
    -- absolute paths.
    --
    -- TODO: Fix

    -- it "prints verbose description of a specification" $ do
    --   (r, ()) <- hCapture [stderr] $ doctest ["--verbose", "TestSimple.Fib"]
    --   r `shouldBe` unlines [
    --       "### Started execution at test/integration/TestSimple/Fib.hs:5."
    --     , "### example:"
    --     , "fib 10"
    --     , "### Successful `test/integration/TestSimple/Fib.hs:5'!"
    --     , ""
    --     , "# Final summary:"
    --     , "Examples: 1  Tried: 1  Errors: 0  Unexpected output: 0"
    --     ]

    -- it "prints verbose description of a property" $ do
    --   (r, ()) <- hCapture [stderr] $ doctest ["--verbose", "PropertyBool.Foo"]
    --   r `shouldBe` unlines [
    --       "### Started execution at test/integration/PropertyBool/Foo.hs:4."
    --     , "### property:"
    --     , "True"
    --     , "### Successful `test/integration/PropertyBool/Foo.hs:4'!"
    --     , ""
    --     , "# Final summary:"
    --     , "Examples: 1  Tried: 1  Errors: 0  Unexpected output: 0"
    --     ]

    -- it "prints verbose error" $ do
    --   (r, e) <- hCapture [stderr] . E.try $ doctest ["--verbose", "Failing.Foo"]
    --   e `shouldBe` Left (ExitFailure 1)
    --   r `shouldBe` unlines [
    --           "### Started execution at test/integration/Failing/Foo.hs:5."
    --         , "### example:"
    --         , "23"
    --         , "test/integration/Failing/Foo.hs:5: failure in expression `23'"
    --         , "expected: 42"
    --         , " but got: 23"
    --         , "          ^"
    --         , ""
    --         , "# Final summary:"
    --         , "Examples: 1  Tried: 1  Errors: 0  Unexpected output: 1"
    --     ]
