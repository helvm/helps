module HelVM.HelPS.Compiler.Compiler where

import qualified Data.List.NonEmpty as NE
import           RIO

defaultCompiler :: Compiler
defaultCompiler = minBound

compilers :: NonEmpty Compiler
compilers = NE.fromList [minBound .. maxBound]

data Compiler = Effectively | Barely | Classy | Typically
  deriving stock (Bounded , Enum , Eq , Read , Show)
