module HelVM.HelPS.CompilerSpec (spec) where

import           HelVM.HelPS.Compiler          (compileText)

import qualified HelVM.HelPS.Compiler.Compiler as Compiler

import           HelVM.HelIO.Extra

import           HelVM.GoldenExpectations

import           Data.Char                     (toLower)

import           Test.Hspec

spec :: Spec
spec = describe "compiler" $
  forM_ Compiler.compilers $ \compiler ->
    let label  = map toLower (show compiler)
        path   = "examples/compiler/standalone/" <> label <> ".hs"
        golden = "compiler/" <> label
    in it label $ (compileText compiler <$> readFileTextUtf8 path) `goldenShouldIO` golden
