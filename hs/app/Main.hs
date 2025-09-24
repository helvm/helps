module Main where

import qualified HelVM.HelPS.Compiler.Adapter    as Compiler
import           HelVM.HelPS.Compiler.How

import qualified HelVM.HelPS.MiniHaskell.Adapter as MH

import qualified HelVM.HelPS.HS2Lazy.Adapter     as HS2Lazy

import qualified HelVM.HelPS.AppOptions          as App

import           HelVM.HelPS.Lang

import           HelVM.HelIO.Control.Safe

import           HelVM.HelIO.Extra

import           Options.Applicative

main :: IO ()
main = run =<< execParser opts where
  opts = info (App.optionParser <**> helper)
      ( fullDesc
     <> header "HelMP: MiniHaskell Compiler"
     <> progDesc "HelMP is a compiler for MiniHaskell, a subset of Haskell, to various esoteric languages.")

run :: App.AppOptions -> IO ()
run o = do
  source <- readFileTextUtf8 $ App.file o
  result <- safeToIO $ runText (App.lang o) (App.how o) source
  putTextLn result

runText :: MonadSafe m => Lang -> How -> Text -> m Text
runText HS2Lazy     _ = HS2Lazy.compileTextSafe
runText MiniHaskell _ = MH.compileTextSafe
runText Compiler    i = Compiler.compileTextSafe i
