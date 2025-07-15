module HelVM.HelPS.CompilerSpec (spec) where

import           HelVM.HelPS.Compiler          (compileText)

import qualified HelVM.HelPS.Compiler.Compiler as Compiler


import           HelVM.HelIO.Extra

import           HelVM.GoldenExpectations

import           Test.Hspec

spec :: Spec
spec = describe "compiler" $ do
  it "typically" $ ( compileText Compiler.Typically <$> readFileTextUtf8 "examples/compiler/standalone/typically.hs") `goldenShouldIO` "compiler/typically"
  it "classy" $ ( compileText Compiler.Classy <$> readFileTextUtf8 "examples/compiler/standalone/classy.hs") `goldenShouldIO` "compiler/classy"
