module HelVM.HelPS.AppOptions where

import           HelVM.HelPS.Compiler.Impl
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
  <*> option auto  (  long    "Impl"
                   <> short   'i'
                   <> metavar "[Impl]"
                   <> help   ("Impl of compiler " <> show impls)
                   <> value    defaultImpl
                   <> showDefault
                   )
  <*> argument str (  metavar "FILE")

data AppOptions = AppOptions
  { lang :: !Lang
  , impl :: !Impl
  , file :: !String
  }
