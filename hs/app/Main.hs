module Main where

import qualified HelVM.HelPS.Compiler                  as C
import           HelVM.HelPS.Compiler.Compiler

import qualified HelVM.HelPS.MiniHaskell.ClassyAdapter as MH

import qualified AppOptions                            as App
import           Data.Text.IO
import           Lang

import           Options.Applicative

import qualified System.IO                             as IO

import           RIO

import           Prelude                               hiding (putStrLn)

main :: IO ()
main = run =<< execParser opts where
  opts = info (App.optionParser <**> helper)
      ( fullDesc
     <> header "HelMP: MiniHaskell Compiler"
     <> progDesc "HelMP is a compiler for MiniHaskell, a subset of Haskell, to various esoteric languages.")

run :: App.AppOptions -> IO ()
run o = do
  source <- readFileUtf8 $ App.file o
  putStrLn $ runText (App.lang o) (App.compiler o) source

runText :: Lang -> Compiler -> Text -> Text
runText MiniHaskell _ = MH.compileText
runText Compiler    c = C.compileText c
