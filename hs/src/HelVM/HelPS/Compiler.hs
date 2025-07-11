module HelVM.HelPS.Compiler where

import           HelVM.HelPS.Compiler.Classy
import           HelVM.HelPS.Util

compileText :: Text -> Text
compileText = mapTextWithString compile
