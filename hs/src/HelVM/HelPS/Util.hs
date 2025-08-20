module HelVM.HelPS.Util where

import           RIO
import qualified RIO.Text as T


mapTextWithString :: (String -> String) -> Text -> Text
mapTextWithString f = T.pack . f . T.unpack
