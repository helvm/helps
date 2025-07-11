module HelVM.HelPS.CompilerSpec (spec) where

import           HelVM.HelPS.Compiler     (compileText)

import           HelVM.HelIO.Extra

import           HelVM.GoldenExpectations

import           Test.Hspec

spec :: Spec
spec = describe "compiler" $ do
  it "classy" $ ( compileText <$> readFileTextUtf8 "examples/compiler/standalone/classy.hs") `goldenShouldIO` "compiler/classy"
