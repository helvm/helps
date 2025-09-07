module HelVM.HelPS.AppOptions where

import           HelVM.HelPS.Compiler.How
import           HelVM.HelPS.Lang

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
  <*> option auto  (  long    "How"
                   <> short   'h'
                   <> metavar "[How]"
                   <> help   ("How compile " <> show hows)
                   <> value    defaultHow
                   <> showDefault
                   )
  <*> argument str (  metavar "FILE")

data AppOptions = AppOptions
  { lang :: !Lang
  , how  :: !How
  , file :: !String
  }
