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
import Control.Error.Util (note)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Zipper as Zipper
import qualified Graphics.Vty as V
import Lens.Micro as Lens
import Safe (atDef, headMay, lastDef)

import qualified AppConfig
import qualified AppInterpState as AIS
import AppState
    ( ActiveWindow (..)
    , AppState (..)
    , appInterpState
    , getSourceContents
    , liveEditor'
    , makeInitialState
    , resetSelectedLine
    , toggleActiveLineInterpreter
    , updateSourceMap
    , writeDebugLog
    )
import AppTopLevel (AppName (..))
import qualified Ghcid.Daemon as Daemon
import qualified Loc
import qualified NameBinding
import Util (showT)
import qualified Util

-- | Alias for 'AppState AppName' convenience.
type AppS = AppState AppName

appDraw :: AppS -> [B.Widget AppName]
appDraw s =
    [ (viewportBox <=> interpreterBox <=> debugBox)
        -- TODO: Make this an expandable viewport, maybe?
        <+> infoBox
    ]
  where
    sourceLabel =
        markLabel
            (s.activeWindow == ActiveCodeViewport)
            ( "Source: "
                <> case s.interpState.pauseLoc of
                    Nothing -> "?"
                    Just loc -> T.pack (Loc.filepath loc)
            )
    interpreterLabel =
        markLabel
            (s.activeWindow == ActiveLiveInterpreter)
            ( if s ^. appInterpState . AIS.viewLock
                then "Interpreter"
                else "Interpreter (Scrolling)"
            )

    viewportBox =
        B.borderWithLabel sourceLabel
            . appendLastCommand
            . B.withVScrollBars B.OnRight
            . B.viewport CodeViewport B.Vertical
            . B.padRight B.Max
            $ codeViewportDraw s
      where
        appendLastCommand w =
            case headMay s.interpState.execHist of
                Just h -> B.padBottom B.Max (w <=> B.hBorder <=> B.txt h)
                _ -> w

    interpreterBox =
        B.borderWithLabel interpreterLabel
            . B.vLimit (displayLimit + 1) -- Plus one for the current line.
            . B.withVScrollBars B.OnRight
            . B.viewport LiveInterpreterViewport B.Vertical
            $ previousOutput <=> lockToBottomOnViewLock promptLine
      where
        enableCursor = True
        displayLimit = 10
        displayF t = B.vBox $ B.txt <$> t
        previousOutput =
            if not (null s.interpLogs)
                then
                    B.txt
                        . T.unlines
                        . reverse
                        $ s.interpLogs
                else B.emptyWidget
        promptLine =
            B.txt s.appConfig.getInterpreterPrompt
                <+> BE.renderEditor displayF enableCursor (s ^. liveEditor')
        lockToBottomOnViewLock w =
            if s ^. appInterpState . AIS.viewLock
                then B.visible w
                else w

    infoBox =
        B.borderWithLabel (B.txt "Info")
            . B.hLimit 30
            . B.padBottom B.Max
            . B.padRight B.Max
            $ case NameBinding.renderNamesTxt s.interpState.bindings of
                [] -> B.txt " " -- Can't be an empty widget due to padding?
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
                                    T.unlines $
                                        reverse logDisplay
            else B.emptyWidget

-- | Mark the label if the first arg is True.
markLabel :: Bool -> T.Text -> B.Widget a
markLabel False labelTxt = B.txt (labelTxt <> " [Ctrl+x]")
markLabel True labelTxt =
    B.withAttr (B.attrName "highlight") (B.txt ("#> " <> labelTxt <> " <#"))

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
    case (currentlyRunning, sourceDataMaybe) of
        (False, _) -> notRunningWidget
        (_, Nothing) -> noSourceWidget
        (_, Just sourceData) -> codeViewportDraw' s sourceData
  where
    currentlyRunning = Daemon.isExecuting (interpState s)
    sourceDataMaybe = getSourceContents s
    padWidget w =
        B.padTop (B.Pad 3)
            . B.hCenter
            $ B.withAttr (B.attrName "styled") w
    splashWidget = maybe (B.txt "No splash file loaded.") B.txt s.splashContents
    notRunningWidget =
        padWidget splashWidget
            <=> padWidget (B.txt "Nothing executing. Maybe run something?")
    noSourceWidget = padWidget splashWidget <=> padWidget (B.txt "Can't display. Source not found.")

-- | Viewport when we have source contents.
codeViewportDraw' :: AppS -> T.Text -> B.Widget AppName
codeViewportDraw' s sourceData = composedTogether
  where
    -- Source data split on lines.
    splitSourceData = T.lines sourceData
    -- Source data split on lines, but only the bit we may want to render.
    windowedSplitSourceData :: [(Int, T.Text)]
    windowedSplitSourceData =
        withLineNums splitSourceData
      where
        -- TODO: Maybe figure this bit out for performance?
        -- We probably can use vpContentSize somehow to ensure
        -- that we don't bother looking at source beyond
        -- the render window.
        startLineno = 1
        _loadedWindowSize = error "loadedWindowSize not implemented"
        withLineNums = zip [startLineno ..]

    breakpoints = Daemon.getBpInCurModule s.interpState
    gutterInfoForLine lineno =
        GutterInfo
            { isStoppedHere =
                s.interpState.pauseLoc
                    <&> Loc.sourceRange
                    <&> (`Loc.isLineInside` lineno)
                    & fromMaybe False
            , isBreakpoint = lineno `elem` breakpoints
            , gutterLineNumber = lineno
            , gutterDigitWidth = Util.getNumDigits $ length splitSourceData
            , isSelected = lineno == s.selectedLine
            }
    prefixLineDefault' (lineno, w) =
        prependGutter
            (gutterInfoForLine lineno)
            w
    originalLookupLineNo =
        s.interpState.pauseLoc
            >>= Loc.startLine . Loc.sourceRange
            & fromMaybe 0

    stoppedLineW :: T.Text -> B.Widget AppName
    stoppedLineW lineTxt =
        let Loc.SourceRange{startCol, endCol} =
                maybe Loc.unknownSourceRange Loc.sourceRange (Daemon.pauseLoc (interpState s))
            lineWidget = makeStoppedLineWidget lineTxt (startCol, endCol)
         in prefixLineDefault' (originalLookupLineNo, lineWidget)

    stoppedRangeW :: Int -> T.Text -> B.Widget AppName
    stoppedRangeW lineno lineTxt =
        prefixLineDefault' (lineno, B.forceAttrAllowStyle (B.attrName "stop-line") (B.txt lineTxt))

    selectedLineW :: T.Text -> B.Widget AppName
    selectedLineW lineTxt =
        let transform = id -- Potentialy useful for highlighting spaces?
            lineWidget = B.txt $ transform lineTxt
         in prefixLineDefault' (s.selectedLine, lineWidget)

    -- Select which line widget we want to draw based on both the interpreter
    -- state and the app state.
    --
    -- It's important that the line information is cached, because
    -- each line is actually pretty expensive to render.
    composedTogetherHelper :: (Int, T.Text) -> B.Widget AppName
    composedTogetherHelper (lineno, lineTxt) = lineWidgetCached
      where
        sr = maybe Loc.unknownSourceRange Loc.sourceRange (Daemon.pauseLoc (interpState s))
        mLineno = Loc.singleify sr
        lineWidget = case mLineno of
            -- This only makes the stopped line widget appear for the start loc.
            Just (singleLine, _) | lineno == singleLine -> stoppedLineW lineTxt
            -- If it's a range, just try to show the range.
            _
                | Loc.isLineInside sr lineno -> stoppedRangeW lineno lineTxt
            -- If it's not something we stopped in, just show the selection normally.
            _
                | lineno == s.selectedLine -> selectedLineW lineTxt
            -- Default case.
            _ -> ((\w -> prefixLineDefault' (lineno, w)) . B.txt) lineTxt
        lineWidgetCached = B.cached (CodeViewportLine lineno) lineWidget

    composedTogether :: B.Widget AppName
    composedTogether =
        B.vBox
            ( (\(num, t) -> wrapSelectedLine num $ composedTogetherHelper (num, t))
                <$> windowedSplitSourceData
            )
      where
        wrapSelectedLine lineno w =
            if lineno == s.selectedLine
                then -- Add highlighting, then mark it as visible in the viewport.
                    B.visible $ B.modifyDefAttr (`V.withStyle` V.bold) w
                else w

-- | Make the Stopped Line widget (the line where we paused execution)
makeStoppedLineWidget :: T.Text -> Loc.ColumnRange -> B.Widget AppName
makeStoppedLineWidget lineData (Nothing, _) =
    B.forceAttrAllowStyle (B.attrName "stop-line") (B.txt lineData)
makeStoppedLineWidget lineData (Just startCol, Nothing) =
    makeStoppedLineWidget lineData (Just startCol, Just (startCol + 1))
makeStoppedLineWidget lineData (Just startCol, Just endCol) =
    B.forceAttrAllowStyle
        (B.attrName "stop-line")
        ( B.txt lineDataBefore
            <+> B.withAttr (B.attrName "highlight") (B.txt lineDataRange)
            <+> B.txt lineDataAfter
        )
  where
    (lineDataBefore, partial) = T.splitAt (startCol - 1) lineData
    (lineDataRange, lineDataAfter) = T.splitAt (endCol - startCol + 1) partial

-- -------------------------------------------------------------------------------------------------
-- Event Handling
-- -------------------------------------------------------------------------------------------------

-- | Handle any Brick event and update the state.
handleEvent :: B.BrickEvent AppName e -> B.EventM AppName AppS ()
handleEvent ev = do
    appState <- B.get
    case appState.activeWindow of
        ActiveCodeViewport -> handleViewportEvent ev
        ActiveLiveInterpreter -> handleInterpreterEvent ev
        _ -> pure ()

-- -------------------------------------------------------------------------------------------------
-- Interpreter Event Handling
-- -------------------------------------------------------------------------------------------------

-- | Handle events when the interpreter (live GHCi) is selected.
handleInterpreterEvent :: B.BrickEvent AppName e -> B.EventM AppName AppS ()
handleInterpreterEvent ev =
    case ev of
        B.VtyEvent (V.EvKey V.KEnter []) -> do
            appState <- B.get
            let cmd = T.strip (T.unlines (editorContents appState))

            -- Actually run the command.
            (newAppState1, output) <- runDaemon2 (`Daemon.execCleaned` cmd) appState

            let newEditor =
                    BE.applyEdit
                        (Zipper.killToEOF . Zipper.gotoBOF)
                        (appState ^. liveEditor')
            -- TODO: Should be configurable?
            let interpreterLogLimit = 1000
            let formattedWithPrompt = appState.appConfig.getInterpreterPrompt <> cmd
            let combinedLogs = reverse output <> (formattedWithPrompt : interpLogs appState)
            let newAppState2 =
                    writeDebugLog ("Handled Enter: Ran '" <> cmd <> "'")
                        . Lens.set (appInterpState . AIS.viewLock) True
                        . Lens.over appInterpState (AIS.pushHistory (editorContents appState))
                        $ newAppState1
                            { interpLogs =
                                take interpreterLogLimit combinedLogs
                            }
            let appStateFinalIO = updateSourceMap (Lens.set liveEditor' newEditor newAppState2)
            B.put =<< liftIO appStateFinalIO
        B.VtyEvent (V.EvKey (V.KChar '\t') []) -> do
            -- Tab completion?
            appState <- B.get
            let cmd = T.strip (T.unlines (editorContents appState))
            (newAppState1, _output) <-
                runDaemon2
                    (`Daemon.execCleaned` (":complete " <> cmd))
                    appState
            B.put newAppState1
        B.VtyEvent (V.EvKey (V.KChar 'x') [V.MCtrl]) ->
            -- Toggle out of the interpreter.
            leaveInterpreter
        B.VtyEvent (V.EvKey V.KEsc _) ->
            -- Also toggle out of the interpreter.
            leaveInterpreter
        B.VtyEvent (V.EvKey V.KUp _) -> do
            let maybeStoreBuffer s =
                    if not (AIS.isScanningHist (getAis s))
                        then storeCommandBuffer s
                        else s
            let wDebug s =
                    writeDebugLog
                        ( "Handled Up; historyPos is "
                            <> (showT . AIS.historyPos . getAis $ s)
                        )
                        s
            appState <- B.get
            let appState' =
                    wDebug
                        . replaceCommandBufferWithHist -- Display the history.
                        . Lens.over appInterpState AIS.pastHistoryPos -- Go back in time.
                        . maybeStoreBuffer -- Store the buffer if we're not scanning already.
                        $ appState
            B.put appState'
        B.VtyEvent (V.EvKey V.KDown _) -> do
            let wDebug s =
                    writeDebugLog
                        ( "Handled Down; historyPos is "
                            <> (showT . AIS.historyPos . getAis $ s)
                        )
                        s
            appState <- B.get
            let appState' =
                    wDebug
                        . replaceCommandBufferWithHist -- Display the history.
                        . Lens.over appInterpState AIS.futHistoryPos -- Go forward in time.
                        $ appState
            B.put appState'
        B.VtyEvent (V.EvKey V.KPageDown _) ->
            B.vScrollPage (B.viewportScroll LiveInterpreterViewport) B.Down
        B.VtyEvent (V.EvKey V.KPageUp _) -> do
            B.vScrollPage (B.viewportScroll LiveInterpreterViewport) B.Up
            appState <- B.get
            B.put (Lens.set (appInterpState . AIS.viewLock) False appState)
        ev' -> do
            appState <- B.get
            B.put (Lens.set (appInterpState . AIS.viewLock) True appState)
            -- Actually handle text input commands.
            B.zoom liveEditor' $ BE.handleEditorEvent ev'
  where
    editorContents appState = BE.getEditContents $ appState ^. liveEditor'
    storeCommandBuffer appState =
        Lens.set (appInterpState . AIS.commandBuffer) (editorContents appState) appState
    getAis s = s ^. appInterpState
    getCommandAtHist :: Int -> AppS -> [T.Text]
    getCommandAtHist i s
        | i <= 0 = s ^. appInterpState . AIS.commandBuffer
        | otherwise = atDef (lastDef [] hist) hist (i - 1)
      where
        hist = s ^. appInterpState . Lens.to AIS.history

    leaveInterpreter = B.put . toggleActiveLineInterpreter =<< B.get

    replaceCommandBufferWithHist :: AppS -> AppS
    replaceCommandBufferWithHist s@AppState{_appInterpState} = replaceCommandBuffer cmd s
      where
        cmd = T.unlines . getCommandAtHist (AIS.historyPos _appInterpState) $ s

-- | Replace the command buffer with the given strings of Text.
replaceCommandBuffer
    :: T.Text
    -- ^ Text to replace with.
    -> AppS
    -- ^ State to modify.
    -> AppS
    -- ^ New state.
replaceCommandBuffer replacement s = Lens.set liveEditor' newEditor s
  where
    zipp :: Zipper.TextZipper T.Text -> Zipper.TextZipper T.Text
    zipp = Zipper.killToEOF . Zipper.insertMany replacement . Zipper.gotoBOF
    newEditor = BE.applyEdit zipp (s ^. liveEditor')

-- -------------------------------------------------------------------------------------------------
-- Viewport Event Handling
-- -------------------------------------------------------------------------------------------------

handleViewportEvent :: B.BrickEvent AppName e -> B.EventM AppName AppS ()
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
                invalidateLineCache
                B.put newState
            | key == V.KChar 'c' -> do
                appState <- B.get
                newState <- Daemon.continue `runDaemon` appState
                invalidateLineCache
                B.put newState
            | key == V.KChar 'b' -> do
                appState <- B.get
                insertViewportBreakpoint appState
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
    moveSelectedLine :: Int -> B.EventM AppName (AppState n) ()
    moveSelectedLine movAmnt = do
        appState <- B.get
        let lineCount = maybe 1 (length . T.lines) (getSourceContents appState)
        let oldLineno = selectedLine appState
        let newLineno = B.clamp 1 lineCount (oldLineno + movAmnt)
        let newState = appState{selectedLine = newLineno}
        -- These two lines need to be re-rendered.
        B.invalidateCacheEntry (CodeViewportLine oldLineno)
        B.invalidateCacheEntry (CodeViewportLine newLineno)
        B.put newState

insertViewportBreakpoint :: AppS -> B.EventM AppName AppS ()
insertViewportBreakpoint appState =
    case selectedModuleLoc appState of
        Left err -> do
            let selectedFileMsg = fromMaybe "<unknown>" appState.selectedFile
            let errMsg =
                    "Cannot find module of line: "
                        <> selectedFileMsg
                        <> ":"
                        <> show appState.selectedLine
                        <> ": "
                        <> T.unpack err
            liftIO $ fail errMsg
        Right ml -> do
            interpState <-
                liftIO $
                    Daemon.toggleBreakpointLine
                        appState.interpState
                        (Daemon.ModLoc ml)
            -- We may need to be smarter about this,
            -- because there's a chance that the module loc 'ml'
            -- doesn't actually refer to this viewed file?
            case Loc.singleify (Loc.sourceRange ml) of
                Just (lineno, _colrange) ->
                    B.invalidateCacheEntry (CodeViewportLine lineno)
                _ ->
                    -- If we don't know, just invalidate everything.
                    invalidateLineCache
            B.put appState{interpState}

-- TODO: Invalidate only the lines instead of the entire application.
invalidateLineCache :: (Ord n) => B.EventM n (state n) ()
invalidateLineCache = B.invalidateCache

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
            B.showCursorNamed widgetName ls
        else -- No cursor
            Nothing
  where
    widgetName = LiveInterpreter

-- | Get Location that's currently selected.
selectedModuleLoc :: AppState n -> Either T.Text Loc.ModuleLoc
selectedModuleLoc s = eModuleLoc =<< fl
  where
    sourceRange = Loc.srFromLineNo (selectedLine s)
    fl = case s.selectedFile of
        Nothing -> Left "No selected file to get module of"
        Just x -> Right (Loc.FileLoc x sourceRange)
    eModuleLoc x =
        let moduleFileMap = Daemon.moduleFileMap (interpState s)
            res = Loc.toModuleLoc moduleFileMap x
            errMsg =
                "No matching module found for '"
                    <> showT x
                    <> "' because moduleFileMap was '"
                    <> showT moduleFileMap
                    <> "'"
         in note errMsg res

-- -------------------------------------------------------------------------------------------------
-- Brick Main
-- -------------------------------------------------------------------------------------------------

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
launchBrick :: AppConfig.AppConfig -> T.Text -> FilePath -> IO ()
launchBrick conf target cwd = do
    initialState <- makeInitialState conf target cwd
    _ <- B.defaultMain brickApp initialState
    pure ()
