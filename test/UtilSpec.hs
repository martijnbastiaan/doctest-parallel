module UtilSpec (main, spec) where

import           Test.Hspec

import           Test.DocTest.Internal.Util

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
  describe "convertDosLineEndings" $ do
    it "converts CRLF to LF" $ do
      convertDosLineEndings "foo\r\nbar\r\nbaz" `shouldBe` "foo\nbar\nbaz"

    it "strips a trailing CR" $ do
      convertDosLineEndings "foo\r" `shouldBe` "foo"

  describe "takeWhileEnd" $ do
    it "returns the longest suffix of elements that satisfy a given predicate" $ do
      takeWhileEnd (/= ' ') "foo bar" `shouldBe` "bar"
