module ProjectsSpec where

import qualified Test.Hspec
import Test.Hspec hiding (it)
import Test.HUnit

import Data.List (isInfixOf)
import Data.Maybe
import System.Environment
import System.Exit
import System.FilePath
import System.IO.Unsafe
import System.Process
import Text.Printf

main :: IO ()
main = hspec spec

type LibName = String
type NumberOfTestsExpected = Word

-- | Run 'cabal run doctests' in a project in projects/, expect a number of
-- succeeded tests.
runProject :: LibName -> NumberOfTestsExpected -> Assertion
runProject libName nTests = do
  (exitCode, _stdout, stderr) <- readCreateProcessWithExitCode process ""
  assertEqual ("'cabal run doctests' succeeded for " <> libName) exitCode ExitSuccess
  assertBool ("expected\n\n" <> expect <> "\n\nin\n\n" <> stderr) (expect `isInfixOf` stderr)
 where
  expect = printf "Examples: %d  Tried: %d  Errors: 0  Unexpected output: 0" nTests nTests
  process = (proc "cabal" ["run", "doctests"])
    { cwd = Just ("test" </> "projects" </> "cpp-options")
    }

-- | 'it' or 'xit', depending on whether we run in a Nix/Stack context. Nix
-- doesn't have Cabal available, so the tests will fail. This is mostly a
-- workaround for CI.
ignoreIfNixOrStack :: Example a => String -> a -> SpecWith (Arg a)
ignoreIfNixOrStack = unsafePerformIO $ do
  stack <- fmap isJust (lookupEnv "STACK_EXE")
  nix <- fmap isJust (lookupEnv "NIX_BUILD_TOP")
  if stack || nix
  then pure Test.Hspec.xit
  else pure Test.Hspec.it


spec :: Spec
spec = do
  ignoreIfNixOrStack "cpp-projects" (runProject "cpp-options" 1)
