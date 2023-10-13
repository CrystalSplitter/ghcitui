{-# LANGUAGE OverloadedStrings #-}

module LocSpec where

import Loc
import Test.Hspec

spec :: Spec
spec = do
    describe "module/file mappings" $ do
        let mfmA = moduleFileMapFromList [("A.Module.Name", "some/filepath")]
        let mfmB = moduleFileMapFromList [("Another.Module.Name", "some/other/filepath")]
        it "can convert a module name to file path" $ do
            getPathOfModule mfmA "A.Module.Name" `shouldBe` Just "some/filepath"
        it "can convert a file path to a module name" $ do
            getModuleOfPath mfmA "some/filepath" `shouldBe` Just "A.Module.Name"
        it "can merge ModuleFileMaps" $ do
            let merged = mfmB <> mfmA
            getModuleOfPath merged "some/other/filepath"
                `shouldBe` Just "Another.Module.Name"
            getModuleOfPath merged "some/filepath"
                `shouldBe` Just "A.Module.Name"
