{-# LANGUAGE OverloadedStrings #-}

module ParseContextSpec where

import Data.Text as T
import Test.Hspec

import qualified Ghcitui.Ghcid.ParseContext as PC
import qualified Ghcitui.Loc as Loc

spec :: Spec
spec = do
    describe "parseContext" $ do
        it "can parse fibonacci context" $ do
            let expectedLoc =
                    Loc.SourceRange
                        { Loc.startLine = Just 9
                        , Loc.startCol = Just 17
                        , Loc.endLine = Just 9
                        , Loc.endCol = Just 29
                        }
            let expected =
                    PC.PCContext (PC.ParseContextOut "Yib.fibty.right" "test/Fib.hs" expectedLoc)
            PC.parseContext fibFixture `shouldBe` expected
        it "can parse the Ormolu function parseModule' (with an apostraphe)" $ do
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


fibFixture :: T.Text
fibFixture =
    T.unlines
        [ "[test/Fib.hs:9:17-29] #~GHCID-START~#[test/Fib.hs:9:17-29] #~GHCID-START~#--> fibty 10"
        , "  Stopped in Yib.fibty.right, test/Fib.hs:9:17-29"
        ]

apostrapheFixture :: T.Text
apostrapheFixture =
    T.unlines
        [ "()"
        , "[src/Ormolu.hs:(257,51)-(261,35)] #~GHCID-START~#()"
        , "[src/Ormolu.hs:(257,51)-(261,35)] #~GHCID-START~#--> invoke"
        , "  Stopped in Ormolu.parseModule', src/Ormolu.hs:(257,51)-(261,35)"
        ]
