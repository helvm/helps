module HelVM.HelPS.Compiler.Compiler where

import qualified Data.List.NonEmpty as NE
import           Relude.Enum
import           RIO

defaultCompiler :: Compiler
defaultCompiler = minBound

compilers :: NonEmpty Compiler
compilers = universeNonEmpty

data Compiler = Effectively | Barely | Classy | Typically
  deriving stock (Bounded , Enum , Eq , Read , Show)
