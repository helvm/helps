module HelVM.HelPS.MiniHaskellSpec (spec) where

import           HelVM.GoldenExpectations
import           HelVM.HelIO.Extra
import           HelVM.HelPS.MiniHaskell.ClassyAdapter (compileText)

import           Test.Hspec

spec :: Spec
spec = describe "mini-haskell" $ do
  it "classy" $ ( compileText <$> readFileTextUtf8 "examples/mini-haskell/standalone/classy.hs") `goldenShouldIO` "mini-haskell/classy"
  it "bignum" $ ( compileText <$> readFileTextUtf8 "examples/mini-haskell/original/bignum.hs") `goldenShouldIO` "mini-haskell/bignum"
