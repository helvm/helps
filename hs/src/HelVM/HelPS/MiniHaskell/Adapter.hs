module HelVM.HelPS.MiniHaskell.Adapter where

import           HelVM.Hel.MiniHaskell.Classy (compile)

import           HelVM.HelPS.Util

compileText :: Text -> Text
compileText = mapTextWithString compile
