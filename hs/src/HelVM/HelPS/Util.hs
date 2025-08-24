module HelVM.HelPS.Util where

mapTextWithString :: (String -> String) -> Text -> Text
mapTextWithString f = toText . f . toString
