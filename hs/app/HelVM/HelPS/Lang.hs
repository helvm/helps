module HelVM.HelPS.Lang where

defaultLang :: Lang
defaultLang = minBound

langs :: NonEmpty Lang
langs = universeNonEmpty

data Lang = HS2Lazy | MiniHaskell | Compiler
  deriving stock (Bounded , Enum , Eq , Read , Show)
