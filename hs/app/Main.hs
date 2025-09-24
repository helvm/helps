module Main where

import qualified HelVM.HelPS.Compiler.Adapter    as Compiler
import           HelVM.HelPS.Compiler.How

import qualified HelVM.HelPS.MiniHaskell.Adapter as MH

import qualified HelVM.HelPS.HS2Lazy.Adapter     as HS2Lazy

import qualified HelVM.HelPS.AppOptions          as App

import           HelVM.HelIO.Extra
import           HelVM.HelPS.Lang

import           Options.Applicative

import qualified Relude.Unsafe                   as Unsafe

main :: IO ()
main = run =<< execParser opts where
  opts = info (App.optionParser <**> helper)
      ( fullDesc
     <> header "HelMP: MiniHaskell Compiler"
     <> progDesc "HelMP is a compiler for MiniHaskell, a subset of Haskell, to various esoteric languages.")

run :: App.AppOptions -> IO ()
run o = do
  source <- readFileTextUtf8 $ App.file o
  putTextLn $ Unsafe.fromJust $ runText (App.lang o) (App.how o) source

runText :: Lang -> How -> Text -> Maybe Text
runText HS2Lazy     _ = HS2Lazy.compileTextMaybe
runText MiniHaskell _ = MH.compileTextMaybe
runText Compiler    i = Compiler.compileTextMaybe i
