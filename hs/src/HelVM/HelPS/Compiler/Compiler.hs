module HelVM.HelPS.Compiler.Compiler where

defaultCompiler :: Compiler
defaultCompiler = minBound

compilers :: NonEmpty Compiler
compilers = universeNonEmpty

data Compiler = Lonely | Effectively | Barely | Classy | Typically
  deriving stock (Bounded , Enum , Eq , Read , Show)
