module HelVM.HelPS.Quest.Compiler (compile) where

import qualified HelVM.HelPS.Quest.Grind.Parity as Parity

import           HelVM.HelIO.Extra

compile :: Int -> String -> IO Text
--compile n filePath = Parity.compile <$> readRemainingLines n filePath

compile n filePath = do
  contents <- readRemainingLines n filePath
  putTextLn contents
  let result = toText $ Parity.compile $ toString contents
  putTextLn result
  pure result

readRemainingLines :: Int -> FilePath -> IO Text
readRemainingLines n filePath = do
  contents <- readFileTextUtf8 filePath
  let allLines = lines contents
  let remainingLines = drop n allLines
  pure (unlines remainingLines)
