module HelVM.HelPS.HS2LazySpec (spec) where

import           HelVM.HelPS.HS2Lazy.Adapter

import           HelVM.HelIO.Control.Safe

import           HelVM.HelIO.Extra

import           HelVM.GoldenExpectations

import           Test.Hspec

import           RIO.FilePath

spec :: Spec
spec = describe "hs2lazy" $
  forM_
    [ "echo"
    , "hello"
    , "even_lines"
    , "tarai"
    , "reverse_lines"
    , "fizzbuzz"
    , "lisp"
    ] $ \filename ->
      let path = "examples" </> "hs2lazy"
          libPath = path </> "libs" </> "hs2lazy-prelude.hs"
          appPath = path </> "apps" </> filename <.> "hs"
          outPath = "hs2lazy" </> filename <.> ".lazy"
          sourceIO = mconcat <$> traverse readFileTextUtf8 [libPath , appPath]
      in it filename $ (compileText <$> sourceIO) `goldenShouldIO` outPath

compileText :: Text -> Text
compileText = unsafe . compileTextSafe
