module Lang where

import qualified Data.List.NonEmpty as NE
import           RIO

defaultLang :: Lang
defaultLang = minBound

langs :: NonEmpty Lang
langs = NE.fromList [minBound .. maxBound]

data Lang = MiniHaskell | Compiler
  deriving stock (Bounded , Enum , Eq , Read , Show)
