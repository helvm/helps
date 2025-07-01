module HelVM.HelPS.MiniHaskell.Compiler where

import           HelVM.HelPS.MiniHaskell.Classy (compile)

compileText :: Text -> Text
compileText = mapTextWithString compile

mapTextWithString :: (String -> String) -> Text -> Text
mapTextWithString f = toText . f . toString
