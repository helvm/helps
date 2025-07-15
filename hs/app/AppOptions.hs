module AppOptions where

import           HelVM.HelPS.Compiler.Compiler

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
  <*> option auto  (  long    "Compiler"
                   <> short   'c'
                   <> metavar "[Compiler]"
                   <> help   ("Level of compiler " <> show compilers)
                   <> value    defaultCompiler
                   <> showDefault
                   )
  <*> argument str (  metavar "FILE")

data AppOptions = AppOptions
  { lang     :: Lang
  , compiler :: Compiler
  , file     :: String
  }
