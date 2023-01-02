module OptionsSpec (spec) where

import           Prelude ()
import           Prelude.Compat

import           Test.Hspec

import           Test.DocTest.Internal.Options
import Test.DocTest.Internal.Logging (LogLevel(..))

spec :: Spec
spec = do
  describe "parseOptions" $ do
    describe "--preserve-it" $ do
      context "without --preserve-it" $ do
        it "does not preserve the `it` variable" $ do
          cfgPreserveIt . cfgModuleConfig <$>
            parseOptions [] `shouldBe` Result False

      context "with --preserve-it" $ do
        it "preserves the `it` variable" $ do
          cfgPreserveIt . cfgModuleConfig <$>
            parseOptions ["--preserve-it"] `shouldBe` Result True

      context "with --no-preserve-it" $ do
        it "preserves the `it` variable" $ do
          cfgPreserveIt . cfgModuleConfig <$>
            parseOptions ["--no-preserve-it"] `shouldBe` Result False

    describe "--randomize-order" $ do
      context "without --randomize-order" $ do
        it "does not set randomize order" $ do
          cfgRandomizeOrder . cfgModuleConfig <$>
            parseOptions [] `shouldBe` Result False

      context "with --randomize-order" $ do
        it "sets randomize order" $ do
          cfgRandomizeOrder . cfgModuleConfig <$>
            parseOptions ["--randomize-order"] `shouldBe` Result True

      context "with --no-randomize-order" $ do
        it "unsets randomize order" $ do
          cfgRandomizeOrder . cfgModuleConfig <$>
            parseOptions ["--no-randomize-order"] `shouldBe` Result False

    context "with --help" $ do
      it "outputs usage information" $ do
        parseOptions ["--help"] `shouldBe` ResultStdout usage

    context "with --version" $ do
      it "outputs version information" $ do
        parseOptions ["--version"] `shouldBe` ResultStdout versionInfo

    context "with --info" $ do
      it "outputs machine readable version information" $ do
        parseOptions ["--info"] `shouldBe` ResultStdout info

    describe "--verbose" $ do
      context "without --verbose" $ do
        it "is not verbose by default" $ do
          cfgLogLevel <$> parseOptions [] `shouldBe` Result Info

      context "with --verbose" $ do
        it "parses verbose option" $ do
          cfgLogLevel <$> parseOptions ["--verbose"] `shouldBe` Result Verbose
