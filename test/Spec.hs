module Main where

import Test.Hspec

import qualified ExtractSpec
import qualified GhciWrapperSpec
import qualified InterpreterSpec
import qualified LocationSpec
import qualified MainSpec
import qualified OptionsSpec
import qualified ParseSpec
import qualified PropertySpec
import qualified RunnerSpec
import qualified RunSpec
import qualified UtilSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "ExtractSpec"     ExtractSpec.spec
  describe "GhciWrapperSpec" GhciWrapperSpec.spec
  describe "InterpreterSpec" InterpreterSpec.spec
  describe "LocationSpec"    LocationSpec.spec
  describe "MainSpec"        MainSpec.spec
  describe "OptionsSpec"     OptionsSpec.spec
  describe "ParseSpec"       ParseSpec.spec
  describe "PropertySpec"    PropertySpec.spec
  describe "RunnerSpec"      RunnerSpec.spec
  describe "RunSpec"         RunSpec.spec
  describe "UtilSpec"        UtilSpec.spec
