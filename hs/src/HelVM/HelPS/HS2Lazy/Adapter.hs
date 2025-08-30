module HelVM.HelPS.HS2Lazy.Adapter where

import           HelVM.HelPS.Util

import qualified HS2Lazy.Run      as Run

compileText :: Text -> Text
compileText = mapTextWithString Run.run
