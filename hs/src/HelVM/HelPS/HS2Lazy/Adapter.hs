module HelVM.HelPS.HS2Lazy.Adapter where

import           HelVM.HelPS.Util

import qualified HelVM.HelPS.HS2Lazy.Facade as Facade

compileTextMaybe :: Text -> Maybe Text
compileTextMaybe = mapTextWithStringF Facade.run
