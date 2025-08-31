module HelVM.HelPS.HS2Lazy.Adapter where

import           HelVM.HelPS.HS2Lazy.Run

import           HelVM.HelPS.Util

compileText :: Text -> Text
compileText = mapTextWithString run
