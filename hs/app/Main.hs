module Main where

import           HelVM.HelPS.Quest.Compiler

main :: IO ()
main = do
--  args <- getArgs
  result <- (compile 28 "hs/src/HelVM/HelPS/Quest/Grind/Parity.hs")
  putTextLn result

