module HelVM.GoldenExpectations (
  goldenShouldIO,
  goldenShouldBe,
) where

import           Control.Type.Operator
import           System.FilePath.Posix

import           Test.Hspec.Core.Spec
import           Test.Hspec.Golden

infix 1 `goldenShouldIO`
goldenShouldIO :: IO String -> FilePath -> GoldenExpectations String
goldenShouldIO actualOutputIO fileName = GoldenExpectations $ flip goldenShouldBe fileName <$> actualOutputIO

infix 1 `goldenShouldBe`
goldenShouldBe :: String -> FilePath -> Golden String
goldenShouldBe actualOutput fileName =
  Golden {
    output = actualOutput,
    encodePretty = show,
    writeToFile = writeFile,
    readFromFile = readFile,
    goldenFile = ".output" </> "golden" </> fileName,
    actualFile = Just (".output" </> "actual" </> fileName),
    failFirstTime = False
  }

----

newtype GoldenExpectations a = GoldenExpectations { unGoldenExpectations :: GoldenIO a }

type GoldenIO a = IO $ Golden a

----

instance Eq str => Example (GoldenExpectations str) where
  type Arg (GoldenExpectations str) = ()
  evaluateExample wrapped params action callback = evaluateExample' =<< unGoldenExpectations wrapped where
    evaluateExample' unwrapped = evaluateExample unwrapped params action callback
