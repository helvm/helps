module HelVM.HelPS.Compiler.How where

defaultHow :: How
defaultHow = minBound

hows :: NonEmpty How
hows = universeNonEmpty

data How = Effectively | Barely | Classy | Typically
  deriving stock (Bounded , Enum , Eq , Read , Show)
