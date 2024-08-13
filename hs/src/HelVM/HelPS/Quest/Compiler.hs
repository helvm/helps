module HelVM.HelPS.Quest.Compiler (compile) where

import qualified HelVM.HelPS.Quest.Grind.Parity as Parity

compile :: Int -> String -> IO String
--compile n filePath = Parity.compile <$> readRemainingLines n filePath

compile n filePath = do
  contents <- readRemainingLines n filePath
  putStrLn contents
  let result = Parity.compile contents
  putStrLn result
  pure result

readRemainingLines :: Int -> FilePath -> IO String
readRemainingLines n filePath = do
  contents <- readFile filePath
  let allLines = lines contents
  let remainingLines = drop n allLines
  pure (unlines remainingLines)
