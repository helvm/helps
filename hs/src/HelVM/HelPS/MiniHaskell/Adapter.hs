module HelVM.HelPS.MiniHaskell.Adapter where

import           HelVM.Hel.MiniHaskell.Classy (compile)

import           HelVM.HelPS.Util

import           UniPatterns

compileTextMaybe :: Text -> Maybe Text
compileTextMaybe = maybeMatch compileText

compileText :: Text -> Text
compileText = mapTextWithString compile
