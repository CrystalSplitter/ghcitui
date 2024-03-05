{-# LANGUAGE OverloadedStrings #-}

module ParseContextSpec where

import Data.Text as T
import Test.Hspec

import qualified Ghcitui.Ghcid.ParseContext as PC
import qualified Ghcitui.Loc as Loc

spec :: Spec
spec = do
    describe "parseContext" $ do
        it "can parse the Ormolu function parseModule' (with an apostraphe)" $ do
            let apostrapheFixture =
                    T.unlines
                        [ "()"
                        , "[src/Ormolu.hs:(257,51)-(261,35)] #~GHCID-START~#()"
                        , "[src/Ormolu.hs:(257,51)-(261,35)] #~GHCID-START~#--> invoke"
                        , "  Stopped in Ormolu.parseModule', src/Ormolu.hs:(257,51)-(261,35)"
                        ]
            let expectedLoc =
                    Loc.SourceRange
                        { Loc.startLine = Just 257
                        , Loc.startCol = Just 51
                        , Loc.endLine = Just 261
                        , Loc.endCol = Just 35
                        }
            let expected =
                    PC.PCContext
                        (PC.ParseContextOut "Ormolu.parseModule'" "src/Ormolu.hs" expectedLoc)
            PC.parseContext apostrapheFixture `shouldBe` expected
