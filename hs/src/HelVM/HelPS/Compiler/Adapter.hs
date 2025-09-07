module HelVM.HelPS.Compiler.Adapter where

import           HelVM.HelPS.Compiler.How

import           HelVM.HelPS.Util

import qualified HelVM.Hel.Compiler.Barely      as Barely
import qualified HelVM.Hel.Compiler.Classy      as Classy
import qualified HelVM.Hel.Compiler.Effectively as Effectively
import qualified HelVM.Hel.Compiler.Typically   as Typically

compileText :: How -> Text -> Text
compileText = mapTextWithString . compile

compile :: How -> String -> String
compile Effectively = Effectively.compile
compile Barely      = Barely.compile
compile Classy      = Classy.compile
compile Typically   = Typically.compile
