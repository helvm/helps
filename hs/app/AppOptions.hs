module AppOptions where

import           Lang
import           Options.Applicative

optionParser :: Parser AppOptions
optionParser = AppOptions
  <$> option auto  (  long    "Lang"
                   <> short   'l'
                   <> metavar "[Lang]"
                   <> help   ("Language to compile " <> show langs)
                   <> value    defaultLang
                   <> showDefault
                   )
  <*> argument str (  metavar "FILE")

data AppOptions = AppOptions
  { lang :: Lang
  , file :: String
  }
