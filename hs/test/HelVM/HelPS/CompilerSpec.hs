module HelVM.HelPS.CompilerSpec (spec) where

import           HelVM.HelPS.Compiler.Adapter (compileText)
import qualified HelVM.HelPS.Compiler.Impl    as Impl

import           HelVM.HelIO.Extra

import           HelVM.GoldenExpectations

import           Data.Char                    (toLower)

import           Test.Hspec

spec :: Spec
spec = describe "compiler" $
  forM_ Impl.impls $ \impl ->
    let label  = map toLower (show impl)
        path   = "examples/compiler/standalone/" <> label <> ".hs"
        golden = "compiler/" <> label
    in it label $ (compileText impl <$> readFileTextUtf8 path) `goldenShouldIO` golden
