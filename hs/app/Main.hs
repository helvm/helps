module Main where

import qualified HelVM.HelPS.Compiler                  as C
import qualified HelVM.HelPS.MiniHaskell.ClassyAdapter as MH

import           HelVM.HelIO.Extra

import qualified AppOptions                            as App
import           Lang

import           Options.Applicative

import qualified System.IO                             as IO

main :: IO ()
main = run =<< execParser opts where
  opts = info (App.optionParser <**> helper)
      ( fullDesc
     <> header "HelMP: MiniHaskell Compiler"
     <> progDesc "HelMP is a compiler for MiniHaskell, a subset of Haskell, to various esoteric languages.")

run :: App.AppOptions -> IO ()
run o = do
  source <- readFileTextUtf8 $ App.file o
  putTextLn $ runText (App.lang o) source


runText :: Lang -> Text -> Text
runText MiniHaskell = MH.compileText
runText Compiler    = C.compileText
