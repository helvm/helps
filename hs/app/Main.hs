module Main where

import           HelVM.HelPS.MiniHaskell.Compiler (compileText)

import           Data.Text.IO                     (interact)

main :: IO ()
main = interact compileText
