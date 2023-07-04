module Main where

import qualified Spec
import           Test.Hspec      (hspec)
import           Test.Hspec.Slow

main :: IO ()
main = configure 1 >>= (hspec . flip timeThese Spec.spec)
