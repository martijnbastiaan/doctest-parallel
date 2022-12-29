{-# LANGUAGE CPP, OverloadedStrings, ImplicitParams #-}
module RunnerSpec (main, spec) where

import Test.Hspec

import Control.Concurrent
import Control.Monad.Trans.State
import System.IO
import System.IO.Silently (hCapture)
import Test.DocTest.Internal.Logging
import Test.DocTest.Internal.Runner
import Text.Printf (printf)

main :: IO ()
main = hspec spec

capture :: Report a -> IO String
capture = fmap fst . hCapture [stderr] . (`execStateT` ReportState 0 True mempty)

-- like capture, but with interactivity set to False
capture_ :: Report a -> IO String
capture_ = fmap fst . hCapture [stderr] . (`execStateT` ReportState 0 False mempty)

spec :: Spec
spec = do
  threadId <- runIO myThreadId
  let ?threadId = threadId
  let ?verbosity = Info

  describe "report" $ do

    context "when mode is interactive" $ do

      it "writes to stderr" $ do
        capture $ do
          report Info "foobar"
        `shouldReturn` printf "[INFO   ] [%s] foobar\n" (show threadId)

      it "overwrites any intermediate output" $ do
        capture $ do
          report_ Info "foo"
          report Info  "bar"
        `shouldReturn` printf "foo\r[INFO   ] [%s] bar\n" (show threadId)

      it "blank out intermediate output if necessary" $ do
        capture $ do
          report_ Info "foobarrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr"
          report Info  "baz"
        `shouldReturn` printf "foobarrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr\r[INFO   ] [%s] baz             \n" (show threadId)

    context "when mode is non-interactive" $ do
      it "writes to stderr" $ do
        capture_ $ do
          report Info "foobar"
        `shouldReturn` printf "[INFO   ] [%s] foobar\n" (show threadId)

  describe "report_ Info" $ do

    context "when mode is interactive" $ do
      it "writes intermediate output to stderr" $ do
        capture $ do
          report_ Info "foobar"
        `shouldReturn` "foobar"

      it "overwrites any intermediate output" $ do
        capture $ do
          report_ Info "foo"
          report_ Info "bar"
        `shouldReturn` "foo\rbar"

      it "blank out intermediate output if necessary" $ do
        capture $ do
          report_ Info "foobar"
          report_ Info  "baz"
        `shouldReturn` "foobar\rbaz   "

    context "when mode is non-interactive" $ do
      it "is ignored" $ do
        capture_ $ do
          report_ Info "foobar"
        `shouldReturn` ""

      it "does not influence a subsequent call to `report Info`" $ do
        capture_ $ do
          report_ Info "foo"
          report Info  "bar"
        `shouldReturn` printf "[INFO   ] [%s] bar\n" (show threadId)

      it "does not require `report Info` to blank out any intermediate output" $ do
        capture_ $ do
          report_ Info "foobar"
          report Info  "baz"
        `shouldReturn` printf "[INFO   ] [%s] baz\n" (show threadId)
