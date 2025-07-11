module Main where

import           HelVM.HelPS.MiniHaskell.ClassyAdapter (compileText)

import           Data.Text.IO                          (interact)

main :: IO ()
main = interact compileText
