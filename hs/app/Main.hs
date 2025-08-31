module Main where

import qualified HelVM.HelPS.Compiler.Adapter          as Compiler
import           HelVM.HelPS.Compiler.Impl

import qualified HelVM.HelPS.MiniHaskell.ClassyAdapter as MH

import qualified HelVM.HelPS.HS2Lazy.Adapter           as HS2Lazy

import qualified HelVM.HelPS.AppOptions                as App

import           HelVM.HelIO.Extra
import           HelVM.HelPS.Lang

import qualified Data.Text.IO                          as IO

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
  putTextLn $ runText (App.lang o) (App.impl o) source

runText :: Lang -> Impl -> Text -> Text
runText HS2Lazy     _ = HS2Lazy.compileText
runText MiniHaskell _ = MH.compileText
runText Compiler    i = Compiler.compileText i
