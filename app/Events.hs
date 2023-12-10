{-# LANGUAGE NamedFieldPuns #-}

module Events (handleEvent, handleCursorPosition) where

import qualified Brick.Main as B
import qualified Brick.Types as B
import qualified Brick.Util as B
import qualified Brick.Widgets.Edit as BE
import Control.Error (atDef, fromMaybe, lastDef, note)
import Control.Monad.IO.Class (MonadIO (..))
import qualified Data.Text as T
import qualified Data.Text.Zipper as T
import qualified Graphics.Vty as V
import Lens.Micro ((^.))
import qualified Lens.Micro as Lens

import qualified AppInterpState as AIS
import AppState
import AppTopLevel
    ( AppName (..)
    )
import qualified Ghcid.Daemon as Daemon
import qualified Loc
import Util (showT)

-- | Handle any Brick event and update the state.
handleEvent :: B.BrickEvent AppName e -> B.EventM AppName (AppState AppName) ()
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
handleInterpreterEvent :: B.BrickEvent AppName e -> B.EventM AppName (AppState AppName) ()
handleInterpreterEvent ev =
    case ev of
        B.VtyEvent (V.EvKey V.KEnter []) -> do
            appState <- B.get
            let cmd = T.strip (T.unlines (editorContents appState))

            -- Actually run the command.
            (newAppState1, output) <- runDaemon2 (Daemon.execCleaned cmd) appState

            let newEditor =
                    BE.applyEdit
                        (T.killToEOF . T.gotoBOF)
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
            -- Invalidate the entire render state of the application
            -- because we don't know what's actually changed here now.
            B.invalidateCache
        B.VtyEvent (V.EvKey (V.KChar '\t') []) -> do
            -- Tab completion?
            appState <- B.get
            let cmd = T.strip (T.unlines (editorContents appState))
            (newAppState1, _output) <-
                runDaemon2
                    (Daemon.execCleaned (":complete " <> cmd))
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
            appState <- B.get
            let wDebug s =
                    writeDebugLog
                        ( "Handled Down; historyPos is "
                            <> (showT . AIS.historyPos . getAis $ s)
                        )
                        s
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
    getCommandAtHist :: Int -> AppState n -> [T.Text]
    getCommandAtHist i s
        | i <= 0 = s ^. appInterpState . AIS.commandBuffer
        | otherwise = atDef (lastDef [] hist) hist (i - 1)
      where
        hist = s ^. appInterpState . Lens.to AIS.cmdHistory

    leaveInterpreter = B.put . toggleActiveLineInterpreter =<< B.get

    replaceCommandBufferWithHist :: AppState n -> AppState n
    replaceCommandBufferWithHist s@AppState{_appInterpState} = replaceCommandBuffer cmd s
      where
        cmd = T.unlines . getCommandAtHist (AIS.historyPos _appInterpState) $ s

-- | Replace the command buffer with the given strings of Text.
replaceCommandBuffer
    :: T.Text
    -- ^ Text to replace with.
    -> AppState n
    -- ^ State to modify.
    -> AppState n
    -- ^ New state.
replaceCommandBuffer replacement s = Lens.set liveEditor' newEditor s
  where
    zipp :: T.TextZipper T.Text -> T.TextZipper T.Text
    zipp = T.killToEOF . T.insertMany replacement . T.gotoBOF
    newEditor = BE.applyEdit zipp (s ^. liveEditor')

-- -------------------------------------------------------------------------------------------------
-- Viewport Event Handling
-- -------------------------------------------------------------------------------------------------

-- TODO: Handle mouse events?
handleViewportEvent :: B.BrickEvent AppName e -> B.EventM AppName (AppState AppName) ()
handleViewportEvent (B.VtyEvent (V.EvKey key ms))
    | key `elem` [V.KChar 'q', V.KEsc] = do
        confirmQuit
    | key == V.KChar 's' = do
        appState <- B.get
        newState <- Daemon.step `runDaemon` appState
        invalidateLineCache
        B.put newState
    | key == V.KChar 'c' = do
        appState <- B.get
        newState <- Daemon.continue `runDaemon` appState
        invalidateLineCache
        B.put newState
    | key == V.KChar 'b' = do
        appState <- B.get
        insertViewportBreakpoint appState
    -- j and k are the vim navigation keybindings.
    | key `elem` [V.KDown, V.KChar 'j'] = do
        moveSelectedLineBy 1
    | key `elem` [V.KUp, V.KChar 'k'] = do
        moveSelectedLineBy (-1)
    | key == V.KPageDown = do
        appState <- B.get
        mViewport <- B.lookupViewport CodeViewport
        let oldSelectedLine = selectedLine appState
        let getViewportBot viewport = B._vpTop viewport + snd (B._vpSize viewport)
        let newSelectedLine = case mViewport of
                -- Need the + 1 due to one-indexing.
                Just viewport ->
                    let lineCount = fromMaybe 1 (getSourceLineCount appState)
                     in B.clamp 1 lineCount (getViewportBot viewport + 1)
                Nothing -> oldSelectedLine
        invalidateCachedLine oldSelectedLine
        invalidateCachedLine newSelectedLine
        B.put appState{selectedLine = newSelectedLine}
        let scroller = B.viewportScroll CodeViewport
        B.vScrollPage scroller B.Down
    | key == V.KPageUp = do
        appState <- B.get
        mViewport <- B.lookupViewport CodeViewport
        let oldSelectedLine = selectedLine appState
        let newSelectedLine = case mViewport of
                Just viewport ->
                    let lineCount = fromMaybe 1 (getSourceLineCount appState)
                     in B.clamp 1 lineCount (B._vpTop viewport)
                Nothing -> oldSelectedLine
        invalidateCachedLine oldSelectedLine
        invalidateCachedLine newSelectedLine
        B.put appState{selectedLine = newSelectedLine}
        let scroller = B.viewportScroll CodeViewport
        B.vScrollPage scroller B.Up
    | key == V.KChar 'x' && ms == [V.MCtrl] =
        B.put . toggleActiveLineInterpreter =<< B.get
handleViewportEvent _ = pure ()

moveSelectedLineBy :: Int -> B.EventM AppName (AppState n) ()
moveSelectedLineBy movAmnt = do
    appState <- B.get
    let lineCount = fromMaybe 1 (getSourceLineCount appState)
    let oldLineno = selectedLine appState
    let newLineno = B.clamp 1 lineCount (oldLineno + movAmnt)
    let newState = appState{selectedLine = newLineno}
    -- These two lines need to be re-rendered.
    invalidateCachedLine oldLineno
    invalidateCachedLine newLineno
    B.put newState

-- TODO: Actually confirm using a dialogue. This is a little tricky,
-- as we may need to store the dialogue structure in the app state.
-- For now, just quit cleanly.
confirmQuit :: B.EventM AppName (AppState AppName) ()
confirmQuit = do
    appState <- B.get
    _ <- liftIO $ Daemon.quit appState.interpState
    B.halt

invalidateCachedLine :: Int -> B.EventM AppName s ()
invalidateCachedLine lineno = B.invalidateCacheEntry (CodeViewportLine lineno)

insertViewportBreakpoint :: AppState AppName -> B.EventM AppName (AppState AppName) ()
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
            let daemonOp = Daemon.toggleBreakpointLine (Daemon.ModLoc ml) appState.interpState
            interpState <-
                liftIO $ do
                    eNewState <- Daemon.run daemonOp
                    case eNewState of
                        Right out -> pure out
                        Left er -> error $ show er
            -- We may need to be smarter about this,
            -- because there's a chance that the module loc 'ml'
            -- doesn't actually refer to this viewed file?
            case Loc.singleify (Loc.sourceRange ml) of
                Just (lineno, _colrange) ->
                    invalidateCachedLine lineno
                _ ->
                    -- If we don't know, just invalidate everything.
                    invalidateLineCache
            B.put appState{interpState}

-- TODO: Invalidate only the lines instead of the entire application.
invalidateLineCache :: (Ord n) => B.EventM n (state n) ()
invalidateLineCache = B.invalidateCache

runDaemon
    :: (MonadIO m)
    => (Daemon.InterpState () -> Daemon.DaemonIO (Daemon.InterpState ()))
    -> AppState n
    -> m (AppState n)
runDaemon f appState =
    liftIO $ do
        interp <-
            (Daemon.run . f) appState.interpState >>= \case
                Right out -> pure out
                Left er -> error $ show er
        newState <- updateSourceMap appState{interpState = interp}
        pure (resetSelectedLine newState)

runDaemon2
    :: (MonadIO m)
    => (Daemon.InterpState () -> Daemon.DaemonIO (Daemon.InterpState (), a))
    -> AppState n
    -> m (AppState n, a)
runDaemon2 f appState =
    liftIO $ do
        (interp, x) <-
            (Daemon.run . f) appState.interpState >>= \case
                Right out -> pure out
                Left er -> error $ show er
        newState <- updateSourceMap appState{interpState = interp}
        pure (resetSelectedLine newState, x)

handleCursorPosition
    :: AppState AppName
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
