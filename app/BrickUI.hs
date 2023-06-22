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
import Debug.Trace (trace)
import qualified Loc

data AppName = GHCiTUI | CodeViewport | LiveInterpreter deriving (Eq, Show, Ord)

-- | Application state wrapper
data AppState = AppState
    { interpState :: Daemon.InterpState ()
    , selectedFile :: Maybe FilePath
    , selectedLine :: Int
    , sourceMap :: Map.Map FilePath Text
    -- ^ Mapping between source filepaths and their contents.
    , appStateConfig :: AppStateConfig
    -- ^ Program launch configuration
    , splashContents :: Maybe Text
    }

resetSelectedLine :: AppState -> AppState
resetSelectedLine s@AppState{interpState} = s{selectedFile, selectedLine}
  where
    selectedLine = fromMaybe 1 interpState.pauseLoc.linenoF
    selectedFile = interpState.pauseLoc.filepath

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
    , isSelected :: !Bool
    , gutterLineNumber :: !Int
    , gutterDigitWidth :: !Int
    }

-- | Prepend gutter information on each line in the primary viewport.
prependGutter :: GutterInfo -> B.Widget n -> B.Widget n
prependGutter gi line = makeGutter gi <+> line

-- | Prepend gutter information on each line in the primary viewport.
makeGutter :: GutterInfo -> B.Widget n
makeGutter GutterInfo{..} =
    lineNoWidget <+> spaceW <+> stopColumn <+> breakColumn <+> spaceW
  where
    spaceW = B.txt " "
    lineNoWidget =
        let attr = B.attrName (if isSelected then "selected-line-numbers" else "line-numbers")
         in B.withAttr attr (B.txt (formatDigits gutterDigitWidth gutterLineNumber))
    breakColumn
        | isSelected && isBreakpoint = B.withAttr (B.attrName "selected-marker") (B.txt "@")
        | isSelected = B.withAttr (B.attrName "selected-marker") (B.txt ">")
        | isBreakpoint = B.withAttr (B.attrName "breakpoint-marker") (B.txt "b")
        | otherwise = spaceW
    stopColumn
        | isStoppedHere = B.withAttr (B.attrName "stop-line") (B.txt "!")
        | otherwise = spaceW

-- | Return the potential contents of the current paused file location.
getSourceContents :: AppState -> Maybe Text
getSourceContents s = s.selectedFile >>= (s.sourceMap Map.!?)

-- | Make the primary viewport widget.
makeCodeViewport :: AppState -> B.Widget AppName
makeCodeViewport s =
    let sourceDataMaybe = getSourceContents s
     in case sourceDataMaybe of
            Nothing ->
                B.padTop (B.Pad 3) $
                    B.hCenter $
                        B.withAttr (B.attrName "styled") $
                            maybe (B.txt "No source file loaded") B.txt s.splashContents
            Just sourceData -> makeCodeViewport' s sourceData

-- | Viewport when we have source contents.
makeCodeViewport' :: AppState -> Text -> B.Widget AppName
makeCodeViewport' s sourceData = composedTogether
  where
    splitSourceData = Data.Text.lines sourceData
    breakpoints = Daemon.getBpInCurModule s.interpState
    gutterInfoForLine lineno =
        GutterInfo
            { isStoppedHere = Just lineno == s.interpState.pauseLoc.linenoF
            , isBreakpoint = lineno `elem` breakpoints
            , gutterLineNumber = lineno
            , gutterDigitWidth = getNumDigits $ length splitSourceData
            , isSelected = lineno == s.selectedLine
            }
    prefixLineDefault' (lineno, w) =
        prependGutter
            (gutterInfoForLine lineno)
            w
    originalLookupLineNo = fromMaybe 0 s.interpState.pauseLoc.linenoF

    stoppedLineW :: Text -> B.Widget AppName
    stoppedLineW lineTxt =
        let lineWidget = makeStoppedLineWidget lineTxt s.interpState.pauseLoc.colrangeF
         in prefixLineDefault' (originalLookupLineNo, lineWidget)

    selectedLineW :: Text -> B.Widget AppName
    selectedLineW lineTxt =
        let lineWidget = B.txt $ Data.Text.replace " " " " lineTxt
         in prefixLineDefault' (s.selectedLine, lineWidget)

    composedTogetherHelper :: (Int, Text) -> B.Widget AppName
    composedTogetherHelper (lineno, lineTxt)
        | Just lineno == s.interpState.pauseLoc.linenoF = stoppedLineW lineTxt
        | lineno == s.selectedLine = selectedLineW lineTxt
        | otherwise =
            ((\w -> prefixLineDefault' (lineno, w)) . B.txt) lineTxt

    composedTogether :: B.Widget AppName
    composedTogether =
        B.vBox
            ( (\(num, t) -> wrapSelectedLine num $ composedTogetherHelper (num, t))
                <$> withLineNums splitSourceData
            )
      where
        wrapSelectedLine lineno w =
            if lineno == s.selectedLine
                then -- Add highilghting, then mark it as visible in the viewport.
                    B.visible $ B.modifyDefAttr (`V.withStyle` V.bold) w
                else w
        withLineNums = zip [1 ..]

-- | Make the Stopped Line widget (the line where we paused execution)
makeStoppedLineWidget :: Text -> Loc.ColumnRange -> B.Widget AppName
makeStoppedLineWidget lineData (Nothing, _) =
    B.forceAttrAllowStyle (B.attrName "stop-line") (B.txt lineData)
makeStoppedLineWidget lineData (Just startCol, Nothing) =
    makeStoppedLineWidget lineData (Just startCol, Just (startCol + 1))
makeStoppedLineWidget lineData (Just startCol, Just endCol) =
    let (lineDataBefore, partial) = Data.Text.splitAt (startCol - 1) lineData
        (lineDataRange, lineDataAfter) = Data.Text.splitAt (endCol - startCol + 1) partial
     in B.forceAttrAllowStyle
            (B.attrName "stop-line")
            ( B.txt lineDataBefore
                <+> B.withAttr (B.attrName "highlight") (B.txt lineDataRange)
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

selectedModuleLoc :: AppState -> Maybe Loc.ModuleLoc
selectedModuleLoc s =
    Loc.toModuleLoc
        s.interpState.moduleFileMap
        (Loc.FileLoc s.selectedFile (Just s.selectedLine) (Nothing, Nothing))

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
                newState <- appState `runDaemon` Daemon.step
                B.put newState
            | key == V.KChar 'c' -> do
                appState <- B.get
                newState <- appState `runDaemon` Daemon.continue
                B.put newState
            | key == V.KChar 'b' -> do
                appState <- B.get
                let mlM = selectedModuleLoc appState
                let runner interpState ml =
                        Daemon.toggleBreakpointLine
                            interpState
                            (Daemon.ModLoc ml)
                case mlM of
                    Nothing ->
                        liftIO $
                            fail
                                ( "Cannot find module of line: "
                                    ++ fromMaybe "<unknown>" appState.selectedFile
                                    ++ ":"
                                    ++ show appState.selectedLine
                                )
                    Just ml -> do
                        interpState <- liftIO $ runner appState.interpState ml
                        B.put appState{interpState}

            -- j and k are the vim navigation keybindings.
            | key `elem` [V.KDown, V.KChar 'j'] -> do
                moveSelectedLine 1
            | key `elem` [V.KUp, V.KChar 'k'] -> do
                moveSelectedLine (-1)
            | key == V.KPageDown -> do
                let scroller = B.viewportScroll CodeViewport
                B.vScrollPage scroller B.Down
                pure ()
            | key == V.KPageUp -> do
                let scroller = B.viewportScroll CodeViewport
                B.vScrollPage scroller B.Up
                pure ()
            | key == V.KChar 'x' && ms == [V.MCtrl] -> do
                -- TODO: Handle live interpreter
                pure ()
        _ -> pure ()
  where
    moveSelectedLine :: Int -> B.EventM a AppState ()
    moveSelectedLine movAmnt = do
        appState <- B.get
        let lineCount = maybe 1 (length . Data.Text.lines) (getSourceContents appState)
        let newState =
                appState{selectedLine = B.clamp 1 lineCount (appState.selectedLine + movAmnt)}
        B.put newState
        pure ()

    runDaemon
        :: (MonadIO m)
        => AppState
        -> (Daemon.InterpState () -> IO (Daemon.InterpState ()))
        -> m AppState
    runDaemon appState f =
        liftIO $ do
            interp <- f appState.interpState
            newState <- updateSourceMap appState{interpState = interp}
            pure $ resetSelectedLine newState

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
                    , (B.attrName "selected-line-numbers", B.fg V.yellow)
                    , (B.attrName "selected-line", B.bg V.brightBlack)
                    , (B.attrName "selected-marker", B.fg V.yellow)
                    , (B.attrName "breakpoint-marker", B.fg V.red)
                    , (B.attrName "underline", B.style V.underline)
                    , (B.attrName "styled", B.fg V.magenta `V.withStyle` V.bold)
                    , (B.attrName "highlight", B.style V.standout)
                    ]
        }

type Command = String

newtype AppStateConfig = AppStateConfig
    { startupSplashPath :: FilePath
    }

-- TODO: This should not be hardcoded for debugging.
makeInitialState :: AppStateConfig -> Command -> IO AppState
makeInitialState config cmd = do
    interpState_ <-
        Daemon.startup cmd "."
            >>= flip Daemon.load "app/Main.hs"
            >>= flip Daemon.stepInto "fibty 10"
    interpState <- Daemon.setBreakpointLine interpState_ (Daemon.LocalLine 41)
    splashContents <- Data.Text.IO.readFile config.startupSplashPath
    pure $
        AppState
            { interpState
            , selectedLine = 1
            , selectedFile = Nothing
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