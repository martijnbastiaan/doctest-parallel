module OptionsSpec (spec) where

import           Prelude ()
import           Prelude.Compat

import           Test.Hspec
import           Test.QuickCheck

import           Options

spec :: Spec
spec = do
  describe "parseOptions" $ do
    let warning = ["WARNING: --optghc is deprecated, doctest now accepts arbitrary GHC options\ndirectly."]
    it "strips --optghc" $
      property $ \xs ys ->
        parseOptions (xs ++ ["--optghc", "foobar"] ++ ys)
        `shouldBe`
        Result (warning, defaultConfig{cfgOptions=xs ++ ["foobar"] ++ ys})

    it "strips --optghc=" $
      property $ \xs ys ->
        parseOptions (xs ++ ["--optghc=foobar"] ++ ys)
        `shouldBe`
        Result (warning, defaultConfig{cfgOptions=xs ++ ["foobar"] ++ ys})

    describe "--no-isolate-modules" $ do
      context "without --no-isolate-modules" $ do
        it "enables module isolation" $ do
          cfgIsolateModules <$> parseOptions [] `shouldBe` Result ([], True)

      context "with --no-isolate-modules" $ do
        it "disables module isolation" $ do
          cfgIsolateModules <$> parseOptions ["--no-isolate-modules"] `shouldBe` Result ([], False)

    describe "--no-magic" $ do
      context "without --no-magic" $ do
        it "enables magic mode" $ do
          cfgMagicMode <$> parseOptions [] `shouldBe` Result ([], True)

      context "with --no-magic" $ do
        it "disables magic mode" $ do
          cfgMagicMode <$> parseOptions ["--no-magic"] `shouldBe` Result ([], False)

    describe "--fast" $ do
      context "without --fast" $ do
        it "disables fast mode" $ do
          cfgFastMode <$> parseOptions [] `shouldBe` Result ([], False)

      context "with --fast" $ do
        it "enabled fast mode" $ do
          cfgFastMode <$> parseOptions ["--fast"] `shouldBe` Result ([], True)

    describe "--preserve-it" $ do
      context "without --preserve-it" $ do
        it "does not preserve the `it` variable" $ do
          cfgPreserveIt <$> parseOptions [] `shouldBe` Result ([], False)

      context "with --preserve-it" $ do
        it "preserves the `it` variable" $ do
          cfgPreserveIt <$> parseOptions ["--preserve-it"] `shouldBe` Result ([], True)

    context "with --help" $ do
      it "outputs usage information" $ do
        parseOptions ["--help"] `shouldBe` Output usage

    context "with --version" $ do
      it "outputs version information" $ do
        parseOptions ["--version"] `shouldBe` Output versionInfo

    context "with --info" $ do
      it "outputs machine readable version information" $ do
        parseOptions ["--info"] `shouldBe` Output info

    describe "--verbose" $ do
      context "without --verbose" $ do
        it "is not verbose by default" $ do
          cfgVerbose <$> parseOptions [] `shouldBe` Result ([], False)

      context "with --verbose" $ do
        it "parses verbose option" $ do
          cfgVerbose <$> parseOptions ["--verbose"] `shouldBe` Result ([], True)
