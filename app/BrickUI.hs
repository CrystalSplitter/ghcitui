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
import qualified Brick.Widgets.Edit as BE
import Control.Monad.IO.Class (MonadIO (..))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text, append, pack, unpack)
import qualified Data.Text
import qualified Data.Text.Zipper as Zipper
import qualified Graphics.Vty as V
import Safe (atMay, headMay)
import qualified Util

import AppState
    ( ActiveWindow (..)
    , AppConfig (..)
    , AppState (..)
    , AppStateConfig (..)
    , getSourceContents
    , liveEditorLens
    , makeInitialState
    , resetSelectedLine
    , toggleActiveLineInterpreter
    , updateSourceMap
    )
import AppTopLevel (AppName (..), Command)
import qualified Daemon
import Debug.Trace (trace)
import qualified Loc
import qualified NameBinding

-- | Alias for 'AppState AppName' convenience.
type AppS = AppState AppName

appDraw :: AppS -> [B.Widget AppName]
appDraw s =
    let
        sourceLabel =
            markLabel
                (s.activeWindow == ActiveCodeViewport)
                ("Source: " `append` maybe "?" pack (s.interpState.pauseLoc.filepath))
        interpreterLabel =
            markLabel (s.activeWindow == ActiveLiveInterpreter) "Interpreter"
        appendLastCommand w =
            case headMay s.interpState.execHist of
                Just h -> B.padBottom B.Max (w <=> B.hBorder <=> B.str h)
                _ -> w
        viewportBox =
            B.borderWithLabel sourceLabel
                . appendLastCommand
                . B.withVScrollBars B.OnRight
                . B.viewport CodeViewport B.Vertical
                . B.padRight B.Max
                $ codeViewportDraw s

        interpreterBox =
            B.borderWithLabel
                interpreterLabel
                $ let
                    enableCursor = True
                    displayLimit = 20
                    displayF t = B.vBox $ B.txt <$> t
                    previousOutput =
                        B.txt
                            . Data.Text.unlines
                            . reverse
                            $ if null s.interpLogs
                                then [" "]
                                else s.interpLogs
                    promptLine =
                        B.txt s.appConfig.interpreterPrompt
                            <+> BE.renderEditor displayF enableCursor s.liveEditor
                    lockToBottom w =
                        if s.liveInterpreterViewLock
                            then B.visible w
                            else w
                   in
                    B.vLimit
                        (displayLimit + 1)
                        ( B.withVScrollBars B.OnRight
                            . B.viewport LiveInterpreterViewport B.Vertical
                            $ (previousOutput <=> lockToBottom promptLine)
                        )
        infoBox =
            B.borderWithLabel (B.txt "Info")
                . B.hLimit 50
                . B.padBottom B.Max
                . B.padRight B.Max
                $ case NameBinding.renderNamesTxt s.interpState.bindings of
                    [] -> B.emptyWidget
                    bs -> B.vBox (B.txt <$> bs)
        debugBox =
            if s.displayDebugConsoleLogs
                then
                    let logDisplay =
                            if null s.debugConsoleLogs then [" "] else s.debugConsoleLogs
                     in B.borderWithLabel (B.txt "Debug") $
                            B.withVScrollBars B.OnRight $
                                B.padRight B.Max $
                                    B.txt $
                                        Data.Text.unlines $
                                            reverse logDisplay
                else B.emptyWidget
     in
        [ (viewportBox <=> interpreterBox <=> debugBox)
            -- TODO: Make this an expandable viewport, maybe?
            <+> infoBox
        ]

-- | Mark the label if the first arg is True.
markLabel :: Bool -> Text -> B.Widget a
markLabel False labelTxt = B.txt labelTxt
markLabel True labelTxt =
    B.withAttr (B.attrName "highlight") (B.txt ("#>" `append` labelTxt `append` "<#"))

-- | Information used to compute the gutter status of each line.
data GutterInfo = GutterInfo
    { isStoppedHere :: !Bool
    -- ^ Is the interpreter stopped/paused here?
    , isBreakpoint :: !Bool
    -- ^ Is there a breakpoint here?
    , isSelected :: !Bool
    -- ^ Is this line currently selected by the user?
    , gutterLineNumber :: !Int
    -- ^ What line number is this?
    , gutterDigitWidth :: !Int
    -- ^ How many columns is the gutter line number?
    }

-- | Prepend gutter information on each line in the primary viewport.
prependGutter :: GutterInfo -> B.Widget n -> B.Widget n
prependGutter gi line = makeGutter gi <+> line

-- | Create the gutter section for a given line (formed from GutterInfo).
makeGutter :: GutterInfo -> B.Widget n
makeGutter GutterInfo{..} =
    lineNoWidget <+> spaceW <+> stopColumn <+> breakColumn <+> spaceW
  where
    spaceW = B.txt " "
    lineNoWidget =
        let attr = B.attrName (if isSelected then "selected-line-numbers" else "line-numbers")
         in B.withAttr attr (B.txt (Util.formatDigits gutterDigitWidth gutterLineNumber))
    breakColumn
        | isSelected && isBreakpoint = B.withAttr (B.attrName "selected-marker") (B.txt "@")
        | isSelected = B.withAttr (B.attrName "selected-marker") (B.txt ">")
        | isBreakpoint = B.withAttr (B.attrName "breakpoint-marker") (B.txt "b")
        | otherwise = spaceW
    stopColumn
        | isStoppedHere = B.withAttr (B.attrName "stop-line") (B.txt "!")
        | otherwise = spaceW

-- | Make the primary viewport widget.
codeViewportDraw :: AppS -> B.Widget AppName
codeViewportDraw s =
    let sourceDataMaybe = getSourceContents s
        noSourceWidget =
            B.padTop (B.Pad 3) $
                B.hCenter $
                    B.withAttr (B.attrName "styled") $
                        maybe (B.txt "No source file loaded") B.txt s.splashContents
     in case sourceDataMaybe of
            Nothing -> noSourceWidget
            Just sourceData -> codeViewportDraw' s sourceData

-- | Viewport when we have source contents.
codeViewportDraw' :: AppS -> Text -> B.Widget AppName
codeViewportDraw' s sourceData = composedTogether
  where
    splitSourceData = Data.Text.lines sourceData
    breakpoints = Daemon.getBpInCurModule s.interpState
    gutterInfoForLine lineno =
        GutterInfo
            { isStoppedHere = Just lineno == s.interpState.pauseLoc.linenoF
            , isBreakpoint = lineno `elem` breakpoints
            , gutterLineNumber = lineno
            , gutterDigitWidth = Util.getNumDigits $ length splitSourceData
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
        let transform = id -- Potentialy useful for highlighting spaces?
            lineWidget = B.txt $ transform lineTxt
         in prefixLineDefault' (s.selectedLine, lineWidget)

    -- Select which line widget we want to draw based on both the interpreter
    -- state and the app state.
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
                then -- Add highlighting, then mark it as visible in the viewport.
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

-- | Get Location that's currently selected.
selectedModuleLoc :: AppState n -> Maybe Loc.ModuleLoc
selectedModuleLoc s =
    Loc.toModuleLoc
        s.interpState.moduleFileMap
        (Loc.FileLoc s.selectedFile (Just s.selectedLine) (Nothing, Nothing))

-- | Handle any Brick event and update the state.
handleEvent :: B.BrickEvent AppName e -> B.EventM AppName AppS ()
handleEvent ev = do
    appState <- B.get
    case appState.activeWindow of
        ActiveCodeViewport -> handleViewportEvent ev
        ActiveLiveInterpreter -> handleInterpreterEvent ev
        _ -> pure ()

-- | Handle events when the interpreter (live GHCi) is selected.
handleInterpreterEvent :: B.BrickEvent AppName e -> B.EventM AppName AppS ()
handleInterpreterEvent ev =
    case ev of
        B.VtyEvent (V.EvKey V.KEnter []) -> do
            appState <- B.get
            let AppState{liveEditor, debugConsoleLogs, interpLogs} = appState
            let cmd = Data.Text.strip . Data.Text.unlines . BE.getEditContents $ liveEditor
            (newAppState, output) <- runDaemon2 (\s -> Daemon.execCleaned s (unpack cmd)) appState
            let newEditor = BE.applyEdit (Zipper.killToEOF . Zipper.gotoBOF) liveEditor
            -- TODO: Should be configurable?
            let interpreterLogLimit = 1000
            let formattedWithPrompt = appState.appConfig.interpreterPrompt `append` cmd
            let combinedLogs = (pack <$> reverse output) ++ formattedWithPrompt : interpLogs
            B.put
                =<< liftIO
                    ( updateSourceMap
                        newAppState
                            { debugConsoleLogs = "Handled Enter" : debugConsoleLogs
                            , liveEditor = newEditor
                            , interpLogs =
                                take interpreterLogLimit combinedLogs
                            , liveInterpreterViewLock = True
                            }
                    )
        B.VtyEvent (V.EvKey (V.KChar 'x') [V.MCtrl]) ->
            -- Toggle out of the interpreter.
            leaveInterpreter
        B.VtyEvent (V.EvKey V.KEsc _) ->
            -- Also toggle out of the interpreter.
            leaveInterpreter
        B.VtyEvent (V.EvKey V.KPageDown _) ->
            B.vScrollPage (B.viewportScroll LiveInterpreterViewport) B.Down
        B.VtyEvent (V.EvKey V.KPageUp _) -> do
            B.vScrollPage (B.viewportScroll LiveInterpreterViewport) B.Up
            appState <- B.get
            B.put appState { liveInterpreterViewLock = False }
        ev' -> do
            appState <- B.get
            B.put appState { liveInterpreterViewLock = True }
            -- Actually handle text input commands.
            B.zoom liveEditorLens $ BE.handleEditorEvent ev'
  where
    leaveInterpreter = B.put . toggleActiveLineInterpreter =<< B.get

handleViewportEvent :: B.BrickEvent AppName e -> B.EventM AppName (AppState n) ()
handleViewportEvent ev =
    case ev of
        B.VtyEvent (V.EvKey key ms)
            | key == V.KChar 'q' -> do
                appState <- B.get
                _ <- liftIO $ Daemon.quit appState.interpState
                B.halt
            | key == V.KChar 's' -> do
                appState <- B.get
                newState <- Daemon.step `runDaemon` appState
                B.put newState
            | key == V.KChar 'c' -> do
                appState <- B.get
                newState <- Daemon.continue `runDaemon` appState
                B.put newState
            | key == V.KChar 'b' -> do
                appState <- B.get
                case selectedModuleLoc appState of
                    Nothing ->
                        liftIO $
                            fail
                                ( "Cannot find module of line: "
                                    ++ fromMaybe "<unknown>" appState.selectedFile
                                    ++ ":"
                                    ++ show appState.selectedLine
                                )
                    Just ml -> do
                        interpState <-
                            liftIO $
                                Daemon.toggleBreakpointLine
                                    appState.interpState
                                    (Daemon.ModLoc ml)
                        B.put appState{interpState}

            -- j and k are the vim navigation keybindings.
            | key `elem` [V.KDown, V.KChar 'j'] -> do
                moveSelectedLine 1
            | key `elem` [V.KUp, V.KChar 'k'] -> do
                moveSelectedLine (-1)
            | key == V.KPageDown -> do
                let scroller = B.viewportScroll CodeViewport
                B.vScrollPage scroller B.Down
            | key == V.KPageUp -> do
                let scroller = B.viewportScroll CodeViewport
                B.vScrollPage scroller B.Up
            | key == V.KChar 'x' && ms == [V.MCtrl] ->
                B.put . toggleActiveLineInterpreter =<< B.get
        -- TODO: Mouse support here?
        _ -> pure ()
  where
    moveSelectedLine :: Int -> B.EventM a (AppState n) ()
    moveSelectedLine movAmnt = do
        appState <- B.get
        let lineCount = maybe 1 (length . Data.Text.lines) (getSourceContents appState)
        let newState =
                appState{selectedLine = B.clamp 1 lineCount (appState.selectedLine + movAmnt)}
        B.put newState
        pure ()

runDaemon
    :: (MonadIO m)
    => (Daemon.InterpState () -> IO (Daemon.InterpState ()))
    -> AppState n
    -> m (AppState n)
runDaemon f appState =
    liftIO $ do
        interp <- f appState.interpState
        newState <- updateSourceMap appState{interpState = interp}
        pure (resetSelectedLine newState)

runDaemon2
    :: (MonadIO m)
    => (Daemon.InterpState () -> IO (Daemon.InterpState (), a))
    -> AppState n
    -> m (AppState n, a)
runDaemon2 f appState =
    liftIO $ do
        (interp, x) <- f appState.interpState
        newState <- updateSourceMap appState{interpState = interp}
        pure (resetSelectedLine newState, x)

handleCursorPosition
    :: AppS
    -- ^ State of the app.
    -> [B.CursorLocation AppName]
    -- ^ Potential Locs
    -> Maybe (B.CursorLocation AppName)
    -- ^ The chosen cursor location if any.
handleCursorPosition s ls =
    if s.activeWindow == ActiveLiveInterpreter
        then -- If we're in the interpreter window, show the cursor.

            let widgetName = LiveInterpreter
             in B.showCursorNamed widgetName ls
        else -- No cursor
            Nothing

-- | Brick main program.
brickApp :: B.App AppS e AppName
brickApp =
    B.App
        { B.appDraw = appDraw
        , B.appChooseCursor = handleCursorPosition
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
