module HelVM.HelPS.HS2Lazy.Adapter where

import           HelVM.HelPS.Util

import qualified HelVM.HelPS.HS2Lazy.Facade as Facade

import           HelVM.HelIO.Control.Safe

compileTextSafe :: MonadSafe m => Text -> m Text
compileTextSafe = mapTextWithStringF Facade.run
