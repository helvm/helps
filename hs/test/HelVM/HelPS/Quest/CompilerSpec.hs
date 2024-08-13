module HelVM.HelPS.Quest.CompilerSpec (spec) where

import           HelVM.HelPS.Quest.Compiler

import           HelVM.GoldenExpectations
import           Test.Hspec

spec :: Spec
spec = describe "compile" $ do
  it "empty" $ "empty" `goldenShouldBe` "empty"
--  it "compile" $ (compile 24 "hs/src/HelVM/HelPS/Quest/Grind/Parity.hs") `goldenShouldIO` "Quest/Grind/Parity"
