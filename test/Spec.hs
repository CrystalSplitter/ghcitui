module Main where

import Test.Hspec

import qualified LocSpec
import qualified StringUtilSpec

main :: IO ()
main = hspec $ do
    LocSpec.spec
    StringUtilSpec.spec
