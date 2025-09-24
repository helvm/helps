module HelVM.HelPS.MiniHaskell.Adapter where

import           HelVM.HelPS.Util

import           HelVM.HelIO.Control.Safe

import           HelVM.Hel.MiniHaskell.Classy (compile)

import           UniPatterns

compileTextSafe :: MonadSafe m => Text -> m Text
compileTextSafe = liftMaybeOrError "MiniHaskell" . compileTextMaybe

compileTextMaybe :: Text -> Maybe Text
compileTextMaybe = maybeMatch compileText

compileText :: Text -> Text
compileText = mapTextWithString compile
