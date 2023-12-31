{-# LANGUAGE OverloadedStrings #-}

module UtilSpec where

import Test.Hspec

import Ghcitui.Util

spec :: Spec
spec = do
    describe "splitBy" $ do
        it "should not edit an empty text" $ do
            splitBy "a" "" `shouldBe` [""]
        it "should not split with an empty delimiter" $ do
            splitBy "" "a" `shouldBe` ["a"]
        it "should split by a delim" $ do
            splitBy "a" "a" `shouldBe` ["", ""]
            splitBy "b" "aba" `shouldBe` ["a", "a"]
