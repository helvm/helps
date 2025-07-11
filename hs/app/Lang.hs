module Lang where

defaultLang :: Lang
defaultLang = minBound

langs :: NonEmpty Lang
langs = universeNonEmpty

data Lang = MiniHaskell | Compiler
  deriving stock (Bounded , Enum , Eq , Read , Show)
