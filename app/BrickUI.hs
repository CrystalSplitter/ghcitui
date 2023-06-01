{-# LANGUAGE OverloadedStrings #-}
module BrickUI (launchBrick) where

import qualified Control.Monad (void)

import qualified Brick.Main as BM
import qualified Brick.Widgets.Core as BC
import qualified Brick.Types as BT
import qualified Brick.AttrMap as BA
import qualified Graphics.Vty as V

import Brick.Widgets.Center (center)
import Data.Text (Text)
import qualified Data.Text

data AppName = GHCiTUI deriving (Eq, Show, Ord)

splash :: Text
splash = Data.Text.unlines [
        ""
    ]

theApp :: BM.App Text e AppName
theApp = BM.App { BM.appDraw = \s -> [center $ BC.txt s]
        , BM.appChooseCursor = BM.neverShowCursor
        , BM.appHandleEvent = const BM.halt
        , BM.appStartEvent = pure ()
        , BM.appAttrMap = const $ BA.attrMap V.defAttr []
    }

launchBrick :: IO ()
launchBrick = do
    let initialState = "Welcome to GHCiTUI!"
    finalState <- BM.defaultMain theApp initialState
    pure ()