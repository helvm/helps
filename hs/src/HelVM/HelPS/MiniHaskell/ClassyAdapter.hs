module HelVM.HelPS.MiniHaskell.ClassyAdapter where

import           HelVM.HelPS.MiniHaskell.Classy (compile)

import           HelVM.HelPS.Util

compileText :: Text -> Text
compileText = mapTextWithString compile
