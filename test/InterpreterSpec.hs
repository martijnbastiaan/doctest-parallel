module InterpreterSpec (main, spec) where

import           Prelude ()
import           Prelude.Compat

import           Test.Hspec

import qualified Test.DocTest.Internal.Interpreter as Interpreter
import           Test.DocTest.Internal.Interpreter
  (haveInterpreterKey, ghcInfo, withInterpreter)
import           Test.DocTest.Internal.Logging (noLogger)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "interpreterSupported" $ do
    it "indicates whether GHCi is supported on current platform" $ do
      (Interpreter.interpreterSupported >> return ()) `shouldReturn` ()

  describe "ghcInfo" $ do
    it ("includes " ++ show haveInterpreterKey) $ do
      info <- ghcInfo
      lookup haveInterpreterKey info `shouldSatisfy`
        (||) <$> (== Just "YES") <*> (== Just "NO")

  describe "safeEval" $ do
    it "evaluates an expression" $ withInterpreter noLogger [] $ \ghci -> do
      Interpreter.safeEval ghci "23 + 42" `shouldReturn` Right "65\n"

    it "returns Left on unterminated multiline command" $ withInterpreter noLogger [] $ \ghci -> do
      Interpreter.safeEval ghci ":{\n23 + 42" `shouldReturn` Left "unterminated multiline command"
