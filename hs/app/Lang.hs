module Lang where

import qualified Data.List.NonEmpty as NE
import           Relude.Enum
import           RIO

defaultLang :: Lang
defaultLang = minBound

langs :: NonEmpty Lang
langs = universeNonEmpty

data Lang = MiniHaskell | Compiler
  deriving stock (Bounded , Enum , Eq , Read , Show)
