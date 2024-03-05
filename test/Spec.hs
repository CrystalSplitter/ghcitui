module Main where

import Test.Hspec

import qualified LocSpec
import qualified ParseContextSpec
import qualified UtilSpec

main :: IO ()
main = hspec $ do
    LocSpec.spec
    UtilSpec.spec
    ParseContextSpec.spec
