module HelVM.HelPS.MiniHaskell.CompilerSpec (spec) where

import           HelVM.GoldenExpectations
import           HelVM.HelIO.Extra
import           HelVM.HelPS.MiniHaskell.Compiler (compileText)

import           Test.Hspec

spec :: Spec
spec = describe "compile" $ do
  it "empty" $ "empty" `goldenShouldBe` "empty"
  it "compile" $ ( compileText <$> readFileTextUtf8 "examples/minihaskell/classy.hs") `goldenShouldIO` "Quest/Grind/Parity"
