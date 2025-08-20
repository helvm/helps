module HelVM.HelPS.MiniHaskell.ClassyAdapter where

import           HelVM.HelPS.MiniHaskell.Classy (compile)

import           HelVM.HelPS.Util

import           RIO

compileText :: Text -> Text
compileText = mapTextWithString compile
