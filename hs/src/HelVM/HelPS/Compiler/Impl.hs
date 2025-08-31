module HelVM.HelPS.Compiler.Impl where

defaultImpl :: Impl
defaultImpl = minBound

impls :: NonEmpty Impl
impls = universeNonEmpty

data Impl = Effectively | Barely | Classy | Typically
  deriving stock (Bounded , Enum , Eq , Read , Show)
