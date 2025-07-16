module HelVM.HelPS.Compiler where

import qualified HelVM.HelPS.Compiler.Classy as Classy
import           HelVM.HelPS.Util

compileText :: Text -> Text
compileText = mapTextWithString compile
