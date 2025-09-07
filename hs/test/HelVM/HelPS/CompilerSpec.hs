module HelVM.HelPS.CompilerSpec (spec) where

import           HelVM.HelPS.Compiler.Adapter (compileText)
import           HelVM.HelPS.Compiler.How

import           HelVM.HelIO.Extra

import           HelVM.GoldenExpectations

import           Data.Char                    (toLower)

import           Test.Hspec

spec :: Spec
spec = describe "compiler" $
  forM_ hows $ \how ->
    let label  = map toLower (show how)
        path   = "examples/compiler/standalone/" <> label <> ".hs"
        golden = "compiler/" <> label
    in it label $ (compileText how <$> readFileTextUtf8 path) `goldenShouldIO` golden
