module HelVM.HelPS.Compiler where

import qualified HelVM.HelPS.Compiler.Compiler           as Compiler

import qualified HelVM.HelPS.Compiler.Compiler.Barely    as Barely
import qualified HelVM.HelPS.Compiler.Compiler.Classy    as Classy
import qualified HelVM.HelPS.Compiler.Compiler.Typically as Typically

import           HelVM.HelPS.Util

compileText :: Compiler.Compiler -> Text -> Text
compileText = mapTextWithString . compile

compile :: Compiler.Compiler -> String -> String
compile Compiler.Barely    = Barely.compile
compile Compiler.Classy    = Classy.compile
compile Compiler.Typically = Typically.compile
