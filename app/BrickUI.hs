{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
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
import Safe

import qualified Daemon

data AppName = GHCiTUI | CodeViewport deriving (Eq, Show, Ord)

data AppState = AppState
    { interpState :: Daemon.InterpState ()
    , sourceMap :: Map.Map FilePath Text
    , appStateConfig :: AppStateConfig
    }

updateSourceMap :: AppState -> IO AppState
updateSourceMap s =
    case s.interpState.filepath of
        Nothing -> pure s
        (Just filepath) -> updateSourceMapWithFilepath s filepath

updateSourceMapWithFilepath :: AppState -> FilePath -> IO AppState
updateSourceMapWithFilepath s filepath
    | Map.member filepath s.sourceMap = pure s
    | otherwise = do
        contents <- Data.Text.IO.readFile filepath
        let newSourceMap = Map.insert filepath contents s.sourceMap
        pure $ s{sourceMap = newSourceMap}

appDraw :: AppState -> [B.Widget AppName]
appDraw s =
    [ B.borderWithLabel
        (B.txt ("Source: " `append` maybe "?" pack (s.interpState.filepath)))
        ( B.withVScrollBars
            B.OnRight
            ( B.viewport
                CodeViewport
                B.Vertical
                ( B.padRight B.Max (makeCodeViewportBoxes s)
                )
            )
        )
        <=> B.borderWithLabel
            (B.txt "Interpreter")
            (B.padRight B.Max (B.txt ">>> "))
    ]

prefixLine :: Int -> B.Widget n -> (Int, B.Widget n) -> B.Widget n
prefixLine digitWidth prefix (idx, lineWidget) =
    B.withAttr
        (B.attrName "line-numbers")
        (B.txt (formatDigits digitWidth idx))
        <+> prefix
        <+> lineWidget

makeCodeViewportBoxes :: AppState -> B.Widget AppName
makeCodeViewportBoxes s =
    case (s.interpState.filepath >>= (s.sourceMap Map.!?) :: Maybe Text) of
        Nothing -> B.txt "No source file loaded"
        Just sourceData ->
            let
                splitSourceData = Data.Text.lines sourceData
                prefixLine' = prefixLine (getNumDigits (length splitSourceData))
                originalLookupLineNo = fromMaybe (-1) s.interpState.lineno - 1
                -- Original Line of Interest
                originalloi =
                    Data.Text.lines sourceData `atMay` originalLookupLineNo

                theLineWidget :: Maybe (B.Widget AppName)
                theLineWidget =
                    ( \x ->
                        let lineWidget = makeLineWidget x s.interpState.colrange
                         in prefixLine' (B.withAttr (B.attrName "stop-line") $ B.txt " > ") (originalLookupLineNo + 1, lineWidget)
                    )
                        <$> originalloi

                -- Surrounding lines in the viewport
                beforeLines =
                    case s.interpState.lineno of
                        Nothing -> mempty
                        Just lineno ->
                            prefixLine' (B.txt "   ")
                                <$> zip
                                    [1 ..]
                                    (B.txt <$> take (max 0 (lineno - 1)) splitSourceData)
                afterLines =
                    case s.interpState.lineno of
                        Nothing -> B.txt <$> Data.Text.lines sourceData
                        Just lineno ->
                            prefixLine' (B.txt "   ")
                                <$> zip
                                    [originalLookupLineNo + 2 ..]
                                    (B.txt <$> drop lineno splitSourceData)

                composedTogether =
                    case theLineWidget of
                        Nothing -> B.vBox afterLines
                        Just w ->
                            B.vBox beforeLines
                                <=> w
                                <=> B.vBox afterLines
             in
                composedTogether

makeLineWidget :: Text -> (Maybe Int, Maybe Int) -> B.Widget AppName
makeLineWidget lineData (Nothing, _) =
    B.withAttr (B.attrName "stop-line") (B.txt lineData)
makeLineWidget lineData (Just startCol, Nothing) =
    makeLineWidget lineData (Just startCol, Just (startCol + 1))
makeLineWidget lineData (Just startCol, Just endCol) =
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
                    ]
        }

type Command = String

data AppStateConfig = AppStateConfig
    { startupSplashPath :: FilePath
    }

makeInitialState :: AppStateConfig -> Command -> IO AppState
makeInitialState config cmd = do
    interpState <-
        Daemon.startup cmd "."
            >>= flip Daemon.load "app/Main.hs"
            >>= flip Daemon.stepInto "fibty 10"
    pure $
        AppState
            { interpState
            , sourceMap = mempty
            , appStateConfig = config
            }

launchBrick :: IO ()
launchBrick = do
    let commandType = "cabal" :: Command
    let cmd = case commandType of
            "cabal" -> "cabal v2-repl ghcitui"
            _ -> error "Not a supported command type"
    initialState <- makeInitialState (AppStateConfig "assets/splash.txt") cmd
    finalState <- B.defaultMain brickApp initialState
    pure ()