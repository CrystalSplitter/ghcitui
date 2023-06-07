{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module BrickUI
    ( launchBrick
    , AppState (..)
    ) where

import qualified Brick as B
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as B
import Brick.Widgets.Core ((<+>), (<=>))
import Control.Monad.IO.Class (MonadIO (..))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text, append, pack)
import qualified Data.Text
import qualified Data.Text.IO
import qualified Graphics.Vty as V
import Safe (atMay)

import qualified Daemon

data AppName = GHCiTUI | CodeViewport | LiveInterpreter deriving (Eq, Show, Ord)

-- | Application state wrapper
data AppState = AppState
    { interpState :: Daemon.InterpState ()
    , sourceMap :: Map.Map FilePath Text
    -- ^ Mapping between source filepaths and their contents.
    , appStateConfig :: AppStateConfig
    -- ^ Program launch configuration
    , splashContents :: Maybe Text
    }

-- | Update the source map given any app state changes.
updateSourceMap :: AppState -> IO AppState
updateSourceMap s =
    case s.interpState.pauseLoc.filepath of
        Nothing -> pure s
        (Just filepath) -> updateSourceMapWithFilepath s filepath

-- | Update the source map with a given filepath.
updateSourceMapWithFilepath :: AppState -> FilePath -> IO AppState
updateSourceMapWithFilepath s filepath
    | Map.member filepath s.sourceMap = pure s
    | otherwise = do
        contents <- Data.Text.IO.readFile filepath
        let newSourceMap = Map.insert filepath contents s.sourceMap
        pure $ s{sourceMap = newSourceMap}

appDraw :: AppState -> [B.Widget AppName]
appDraw s =
    [ ( B.borderWithLabel
            (B.txt ("Source: " `append` maybe "?" pack (s.interpState.pauseLoc.filepath)))
            ( B.withVScrollBars
                B.OnRight
                ( B.viewport
                    CodeViewport
                    B.Vertical
                    ( B.padRight B.Max (makeCodeViewport s)
                    )
                )
            )
            -- TODO: Make this an editor viewport.
            <=> B.borderWithLabel
                (B.txt "Interpreter")
                (B.padRight B.Max (B.txt " >>> "))
      )
        -- TODO: Make this an expandable viewport, maybe?
        <+> B.borderWithLabel
            (B.txt "Info")
            ( B.padBottom
                B.Max
                ( B.padLeft
                    (B.Pad 20)
                    (B.txt " ") -- Important that there's a space here for padding.
                )
            )
    ]

data GutterInfo = GutterInfo
    { isStoppedHere :: !Bool
    , isBreakpoint :: !Bool
    , gutterLineNumber :: !Int
    , gutterDigitWidth :: !Int
    }

-- | Prepend gutter information on each line in the primary viewport.
prependGutter :: GutterInfo -> B.Widget n -> B.Widget n
prependGutter gi line = makeGutter gi <+> line

-- | Prepend gutter information on each line in the primary viewport.
makeGutter :: GutterInfo -> B.Widget n
makeGutter GutterInfo{..} = lineNoWidget <+> emptyW <+> breakColumn <+> stopColumn <+> emptyW
  where
    emptyW = B.txt " "
    lineNoWidget =
        B.withAttr
            (B.attrName "line-numbers")
            (B.txt (formatDigits gutterDigitWidth gutterLineNumber))
    breakColumn
        | isBreakpoint =
            B.withAttr
                (B.attrName "breakpoint-marker")
                (B.txt "@")
        | otherwise = emptyW
    stopColumn
        | isStoppedHere = B.withAttr (B.attrName "stop-line") (B.txt ">")
        | otherwise = emptyW

-- | Make the primary viewport widget.
makeCodeViewport :: AppState -> B.Widget AppName
makeCodeViewport s =
    case (s.interpState.pauseLoc.filepath >>= (s.sourceMap Map.!?) :: Maybe Text) of
        Nothing ->
            B.padTop (B.Pad 3) $
                B.hCenter $
                    B.withAttr (B.attrName "styled") $
                        maybe (B.txt "No source file loaded") B.txt s.splashContents
        Just sourceData ->
            let
                splitSourceData = Data.Text.lines sourceData
                gutterInfoForLine lineno =
                    GutterInfo
                        { isStoppedHere = Just lineno == s.interpState.pauseLoc.lineno
                        , isBreakpoint = lineno `elem` Daemon.getBpInCurFile s.interpState
                        , gutterLineNumber = lineno
                        , gutterDigitWidth = getNumDigits $ length splitSourceData
                        }
                prefixLineDefault' (lineno, w) =
                    prependGutter
                        (gutterInfoForLine lineno)
                        w
                originalLookupLineNo = fromMaybe (-1) s.interpState.pauseLoc.lineno - 1
                -- Original Line of Interest
                originalloi =
                    Data.Text.lines sourceData `atMay` originalLookupLineNo

                stoppedLineW :: Text -> B.Widget AppName
                stoppedLineW lineTxt =
                    let lineWidget = makeStoppedLineWidget lineTxt s.interpState.pauseLoc.colrange
                     in prependGutter (gutterInfoForLine (originalLookupLineNo + 1)) lineWidget

                -- Surrounding lines in the viewport
                beforeLines =
                    case s.interpState.pauseLoc.lineno of
                        Nothing -> mempty
                        Just lineno ->
                            prefixLineDefault'
                                <$> zip
                                    [1 ..]
                                    (B.txt <$> take (max 0 (lineno - 1)) splitSourceData)
                afterLines =
                    case s.interpState.pauseLoc.lineno of
                        Nothing -> B.txt <$> Data.Text.lines sourceData
                        Just lineno ->
                            prefixLineDefault'
                                <$> zip
                                    [originalLookupLineNo + 2 ..]
                                    (B.txt <$> drop lineno splitSourceData)

                composedTogether =
                    case stoppedLineW <$> originalloi of
                        Nothing -> B.vBox afterLines
                        Just w ->
                            B.vBox beforeLines
                                <=> w
                                <=> B.vBox afterLines
             in
                composedTogether

-- | Make the Stopped Line widget (the line where we paused execution)
makeStoppedLineWidget :: Text -> (Maybe Int, Maybe Int) -> B.Widget AppName
makeStoppedLineWidget lineData (Nothing, _) =
    B.withAttr (B.attrName "stop-line") (B.txt lineData)
makeStoppedLineWidget lineData (Just startCol, Nothing) =
    makeStoppedLineWidget lineData (Just startCol, Just (startCol + 1))
makeStoppedLineWidget lineData (Just startCol, Just endCol) =
    let (lineDataBefore, partial) = Data.Text.splitAt (startCol - 1) lineData
        (lineDataRange, lineDataAfter) = Data.Text.splitAt (endCol - startCol + 1) partial
     in B.withAttr
            (B.attrName "stop-line")
            ( B.txt lineDataBefore
                <+> B.withAttr (B.attrName "underline") (B.txt lineDataRange)
                <+> B.txt lineDataAfter
            )

-- Return the number of digits in a given integral
getNumDigits :: (Integral a) => a -> Int
getNumDigits 0 = 1
getNumDigits num = truncate (logBase 10 (fromIntegral num) :: Double) + 1

formatDigits
    :: Int
    -- ^ Number of spaces
    -> Int
    -- ^ Number to format digits of
    -> Text
    -- ^ Formatted Text
formatDigits spacing num = pack (replicate left ' ') `append` pack (show num)
  where
    left = spacing - getNumDigits num

handleEvent :: B.BrickEvent AppName e -> B.EventM AppName AppState ()
handleEvent ev =
    case ev of
        B.VtyEvent (V.EvKey key ms)
            | key == V.KChar 'q' -> do
                appState <- B.get
                _ <- liftIO $ Daemon.quit appState.interpState
                B.halt
            | key == V.KChar 's' -> do
                appState <- B.get
                newState <- liftIO $ do
                    interp <- Daemon.step appState.interpState
                    pure $ appState{interpState = interp}
                newState <- liftIO $ updateSourceMap newState
                B.put newState
                pure ()
            | key == V.KDown -> do
                let scroller = B.viewportScroll CodeViewport
                B.vScrollBy scroller 1
                pure ()
            | key == V.KUp -> do
                let scroller = B.viewportScroll CodeViewport
                B.vScrollBy scroller (-1)
                pure ()
            | key == V.KPageDown -> do
                let scroller = B.viewportScroll CodeViewport
                B.vScrollPage scroller B.Down
                pure ()
            | key == V.KPageUp -> do
                let scroller = B.viewportScroll CodeViewport
                B.vScrollPage scroller B.Up
                pure ()
            | key == V.KChar 'x' && ms == [V.MCtrl] -> do
                pure ()
        _ -> pure ()

-- | Brick main program.
brickApp :: B.App AppState e AppName
brickApp =
    B.App
        { B.appDraw = appDraw
        , B.appChooseCursor = B.neverShowCursor
        , B.appHandleEvent = handleEvent
        , B.appStartEvent = pure ()
        , B.appAttrMap =
            const $
                B.attrMap
                    V.defAttr
                    [ (B.attrName "stop-line", B.fg V.red)
                    , (B.attrName "line-numbers", B.fg V.cyan)
                    , (B.attrName "underline", V.currentAttr `V.withStyle` V.underline)
                    , (B.attrName "styled", B.fg V.magenta)
                    ]
        }

type Command = String

newtype AppStateConfig = AppStateConfig
    { startupSplashPath :: FilePath
    }

-- TODO: This should not be hardcoded for debugging.
makeInitialState :: AppStateConfig -> Command -> IO AppState
makeInitialState config cmd = do
    interpState <-
        Daemon.startup cmd "."
            >>= flip Daemon.load "app/Main.hs"
            >>= flip Daemon.stepInto "fibty 10"
    splashContents <- Data.Text.IO.readFile config.startupSplashPath
    pure $
        AppState
            { interpState
            , sourceMap = mempty
            , appStateConfig = config
            , splashContents = Just splashContents
            }

-- | Start the Brick UI
launchBrick :: IO ()
launchBrick = do
    let commandType = "cabal" :: Command
    let cmd = case commandType of
            "cabal" -> "cabal v2-repl ghcitui"
            _ -> error "Not a supported command type"
    initialState <- makeInitialState (AppStateConfig "assets/splash.txt") cmd
    _ <- B.defaultMain brickApp initialState
    pure ()