module HelVM.HelPS.Compiler.Adapter where

import qualified HelVM.HelPS.Compiler.Impl             as Impl

import qualified HelVM.HelPS.Compiler.Impl.Barely      as Barely
import qualified HelVM.HelPS.Compiler.Impl.Classy      as Classy
import qualified HelVM.HelPS.Compiler.Impl.Effectively as Effectively
import qualified HelVM.HelPS.Compiler.Impl.Typically   as Typically

import           HelVM.HelPS.Util

compileText :: Impl.Impl -> Text -> Text
compileText = mapTextWithString . compile

compile :: Impl.Impl -> String -> String
compile Impl.Effectively = Effectively.compile
compile Impl.Barely      = Barely.compile
compile Impl.Classy      = Classy.compile
compile Impl.Typically   = Typically.compile
