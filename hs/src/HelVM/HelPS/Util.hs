module HelVM.HelPS.Util where

import           Control.Applicative.Tools

mapTextWithString :: (String -> String) -> Text -> Text
mapTextWithString f = toText . f . toString

mapTextWithStringF :: Functor f => (String -> f String) -> Text -> f Text
mapTextWithStringF f = toText <.> f . toString
