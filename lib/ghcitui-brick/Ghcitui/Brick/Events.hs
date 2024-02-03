{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Ghcitui.Brick.Events (handleEvent, handleCursorPosition) where

import qualified Brick.Main as B
import qualified Brick.Types as B
import qualified Brick.Widgets.Edit as BE
import Control.Category ((>>>))
import Control.Error (atDef, fromMaybe, lastDef, note)
import Control.Monad.IO.Class (MonadIO (..))
import Data.List (foldl1')
import qualified Data.Text as T
import qualified Data.Text.Zipper as T
import qualified Graphics.Vty as V
import Lens.Micro ((^.))
import qualified Lens.Micro as Lens

import qualified Ghcitui.Brick.AppInterpState as AIS
import Ghcitui.Brick.AppState as AppState
import Ghcitui.Brick.AppTopLevel
    ( AppName (..)
    )
import qualified Ghcitui.Brick.SourceWindow as SourceWindow
import qualified Ghcitui.Ghcid.Daemon as Daemon
import qualified Ghcitui.Loc as Loc
import Ghcitui.Util (showT)

-- | Handle any Brick event and update the state.
handleEvent :: B.BrickEvent AppName e -> B.EventM AppName (AppState AppName) ()
handleEvent (B.VtyEvent (V.EvResize _ _)) = B.invalidateCache
handleEvent ev = do
    appState <- B.get
    updatedSourceWindow <- SourceWindow.updateSrcWindowEnd (appState ^. AppState.sourceWindow)
    let appStateUpdated = Lens.set AppState.sourceWindow updatedSourceWindow appState
    let handler = case appStateUpdated.activeWindow of
            AppState.ActiveCodeViewport -> handleSrcWindowEvent
            AppState.ActiveLiveInterpreter -> handleInterpreterEvent
            AppState.ActiveInfoWindow -> handleInfoEvent
            AppState.ActiveDialogQuit -> handleDialogQuit
            AppState.ActiveDialogHelp -> handleDialogHelp
    handler ev

-- -------------------------------------------------------------------------------------------------
-- Info Event Handling
----------------------------------------------------------------------------------------------------

handleInfoEvent :: B.BrickEvent AppName e -> B.EventM AppName (AppState AppName) ()
handleInfoEvent ev = do
    appState <- B.get
    case ev of
        B.VtyEvent (V.EvKey key _ms)
            | key `elem` [V.KChar 'j', V.KDown] -> do
                B.put $ AppState.changeSelectedModuleInInfoPanel 1 appState
            | key `elem` [V.KChar 'k', V.KUp] -> do
                B.put $ AppState.changeSelectedModuleInInfoPanel (-1) appState
            | key == V.KEnter || key == V.KChar 'o' -> do
                let mayFp = AppState.filePathOfInfoSelectedModule appState
                case mayFp of
                    Just _ -> do
                        updatedState <- liftIO $ AppState.setSelectedFile mayFp appState
                        B.put updatedState
                        invalidateLineCache
                    Nothing -> pure ()
            | key == V.KEsc || key == V.KChar 'C' -> do
                B.put appState{activeWindow = AppState.ActiveCodeViewport}
        B.VtyEvent (V.EvKey (V.KChar 'x') [V.MCtrl]) -> do
            B.put appState{activeWindow = AppState.ActiveLiveInterpreter}
        B.VtyEvent (V.EvKey (V.KChar '?') _) -> do
            B.put appState{activeWindow = AppState.ActiveDialogHelp}

        -- Resizing
        B.VtyEvent (V.EvKey (V.KChar '-') []) -> do
            B.put (AppState.changeInfoWidgetSize (-1) appState)
            invalidateLineCache
        B.VtyEvent (V.EvKey (V.KChar '+') []) -> do
            B.put (AppState.changeInfoWidgetSize 1 appState)
            invalidateLineCache
        _ -> pure ()
    B.invalidateCacheEntry ModulesViewport

-- -------------------------------------------------------------------------------------------------
-- Interpreter Event Handling
-- -------------------------------------------------------------------------------------------------

-- | Handle events when the interpreter (live GHCi) is selected.
handleInterpreterEvent :: B.BrickEvent AppName e -> B.EventM AppName (AppState AppName) ()
handleInterpreterEvent ev = do
    appState <- B.get
    case ev of
        B.VtyEvent (V.EvKey V.KEnter []) -> do
            let cmd = T.strip (T.unlines (editorContents appState))

            -- Actually run the command.
            (newAppState1, output) <- runDaemon2 (Daemon.execCleaned cmd) appState

            let newEditor =
                    BE.applyEdit
                        (T.killToEOF . T.gotoBOF)
                        (appState ^. liveEditor)
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
            let appStateFinalIO = updateSourceMap (Lens.set liveEditor newEditor newAppState2)
            B.put =<< liftIO appStateFinalIO
            -- Invalidate the entire render state of the application
            -- because we don't know what's actually changed here now.
            B.invalidateCache
        B.VtyEvent (V.EvKey (V.KChar '\t') []) -> do
            let input = T.strip (T.unlines (editorContents appState))
                -- Tab completion expects input to be 'show'n as a String
                -- there's probably a better way of doing this!
                inputShown = T.pack $ show $ T.unpack input
                cmd = ":complete repl " <> inputShown
            (newAppState1, output) <-
                runDaemon2
                    (Daemon.execCleaned cmd)
                    appState
            let (completions, beforeText) = case output of
                    (context : completions')
                        | (toPrint : _total : rest) <- T.splitOn " " context ->
                            let toPrint' = read (T.unpack toPrint) :: Int
                             in ( T.pack . read . T.unpack <$> take toPrint' completions'
                                , -- need to restore spaces which were split apart
                                  T.pack . read . T.unpack $ T.intercalate " " rest
                                )
                    _ ->
                        error $
                            "Unexpected :complete result from GHCi: " ++ show output

                formattedWithPrompt = appState.appConfig.getInterpreterPrompt <> input
                combinedLogs =
                    -- only show completions if there is more than one
                    [T.intercalate " " completions | length completions > 1]
                        <> (formattedWithPrompt : interpLogs appState)
                interpreterLogLimit = 1000

                commonPrefix =
                    if null completions
                        then ""
                        else foldl1' commonPrefixes completions
                newEditor =
                    BE.applyEdit
                        ( T.gotoBOF
                            >>> T.killToEOF
                            >>> T.insertMany beforeText
                            >>> T.insertMany commonPrefix
                        )
                        (appState ^. liveEditor)
            B.put
                . writeDebugLog ("Handled Tab: Ran '" <> cmd <> "'")
                . Lens.set liveEditor newEditor
                $ newAppState1
                    { interpLogs =
                        take interpreterLogLimit combinedLogs
                    }
        B.VtyEvent (V.EvKey (V.KChar 'x') [V.MCtrl]) ->
            -- Toggle out of the interpreter.
            leaveInterpreter
        B.VtyEvent (V.EvKey V.KEsc _) -> do
            if not $ appState ^. appInterpState . AIS.viewLock
                then -- Exit scroll mode first.
                    B.put (Lens.set (appInterpState . AIS.viewLock) True appState)
                else -- Also toggle out of the interpreter.
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
            B.put (Lens.set (appInterpState . AIS.viewLock) False appState)
        B.VtyEvent (V.EvKey (V.KChar 'n') [V.MCtrl]) -> do
            -- Invert the viewLock.
            B.put (Lens.over (appInterpState . AIS.viewLock) not appState)

        -- While scrolling (viewLock disabled), allow resizing the live interpreter history.
        B.VtyEvent (V.EvKey (V.KChar '+') [])
            | not (appState ^. appInterpState . AIS.viewLock) -> do
                B.put (AppState.changeReplWidgetSize 1 appState)
        B.VtyEvent (V.EvKey (V.KChar '-') [])
            | not (appState ^. appInterpState . AIS.viewLock) -> do
                B.put (AppState.changeReplWidgetSize (-1) appState)

        -- Actually handle keystrokes.
        ev' -> do
            -- When typing, bring us back down to the terminal.
            B.put (Lens.set (appInterpState . AIS.viewLock) True appState)
            -- Actually handle text input commands.
            B.zoom liveEditor $ BE.handleEditorEvent ev'
  where
    editorContents appState = BE.getEditContents $ appState ^. liveEditor
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

    commonPrefixes :: T.Text -> T.Text -> T.Text
    commonPrefixes t u =
        case T.commonPrefixes t u of
            Just (p, _, _) -> p
            Nothing -> ""

-- | Replace the command buffer with the given strings of Text.
replaceCommandBuffer
    :: T.Text
    -- ^ Text to replace with.
    -> AppState n
    -- ^ State to modify.
    -> AppState n
    -- ^ New state.
replaceCommandBuffer replacement s = Lens.set liveEditor newEditor s
  where
    zipp :: T.TextZipper T.Text -> T.TextZipper T.Text
    zipp = T.killToEOF . T.insertMany replacement . T.gotoBOF
    newEditor = BE.applyEdit zipp (s ^. liveEditor)

-- -------------------------------------------------------------------------------------------------
-- Code Viewport Event Handling
-- -------------------------------------------------------------------------------------------------

-- TODO: Handle mouse events?
handleSrcWindowEvent :: B.BrickEvent AppName e -> B.EventM AppName (AppState AppName) ()
handleSrcWindowEvent (B.VtyEvent (V.EvKey key ms))
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
    | key == V.KChar 't' = do
        appState <- B.get
        newState <- Daemon.trace `runDaemon` appState
        invalidateLineCache
        B.put newState
    | key == V.KChar 'b' = do
        appState <- B.get
        insertBreakpoint appState

    -- j and k are the vim navigation keybindings.
    | key `elem` [V.KDown, V.KChar 'j'] = do
        moveSelectedLineby 1
    | key `elem` [V.KUp, V.KChar 'k'] = do
        moveSelectedLineby (-1)
    | key == V.KPageDown = do
        scrollPage SourceWindow.Down
    | key == V.KPageUp = do
        scrollPage SourceWindow.Up

    -- '+' and '-' move the middle border.
    | key == V.KChar '+' && null ms = do
        appState <- B.get
        B.put (AppState.changeInfoWidgetSize (-1) appState)
        B.invalidateCacheEntry ModulesViewport
        invalidateLineCache
    | key == V.KChar '-' && null ms = do
        appState <- B.get
        B.put (AppState.changeInfoWidgetSize 1 appState)
        B.invalidateCacheEntry ModulesViewport
        invalidateLineCache
    | key == V.KChar 'x' && ms == [V.MCtrl] =
        B.put . toggleActiveLineInterpreter =<< B.get
    | key == V.KChar 'M' = do
        appState <- B.get
        B.put appState{activeWindow = AppState.ActiveInfoWindow}
        B.invalidateCacheEntry ModulesViewport
    | key == V.KChar '?' = B.modify (\state -> state{activeWindow = AppState.ActiveDialogHelp})
handleSrcWindowEvent _ = pure ()

moveSelectedLineby :: Int -> B.EventM AppName (AppState AppName) ()
moveSelectedLineby movAmnt = do
    appState <- B.get
    let oldLineno = AppState.selectedLine appState
    movedAppState <- do
        sw <- SourceWindow.srcWindowMoveSelectionBy movAmnt (appState ^. AppState.sourceWindow)
        pure $ Lens.set AppState.sourceWindow sw appState
    let newLineno = AppState.selectedLine movedAppState
    -- These two lines need to be re-rendered.
    invalidateCachedLine oldLineno
    invalidateCachedLine newLineno
    B.put $ writeDebugLog ("Selected line is: " <> showT newLineno) movedAppState

scrollPage :: SourceWindow.ScrollDir -> B.EventM AppName (AppState AppName) ()
scrollPage dir = do
    appState <- B.get
    B.put
        . (\srcW -> Lens.set AppState.sourceWindow srcW appState)
        =<< SourceWindow.srcWindowScrollPage dir (appState ^. AppState.sourceWindow)
    invalidateLineCache

-- | Open up the quit dialog. See 'quit' for the actual quitting.
confirmQuit :: B.EventM AppName (AppState AppName) ()
confirmQuit = B.put . (\s -> s{activeWindow = AppState.ActiveDialogQuit}) =<< B.get

invalidateCachedLine :: Int -> B.EventM AppName s ()
invalidateCachedLine lineno = B.invalidateCacheEntry (SourceWindowLine lineno)

insertBreakpoint :: AppState AppName -> B.EventM AppName (AppState AppName) ()
insertBreakpoint appState =
    case selectedModuleLoc appState of
        Left err -> do
            let selectedFileMsg = fromMaybe "<unknown>" (selectedFile appState)
            let errMsg =
                    "Cannot find module of line: "
                        <> selectedFileMsg
                        <> ":"
                        <> show (selectedLine appState)
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

-- | Run a DaemonIO function on a given interpreter state, within an EventM monad.
runDaemon
    :: (Ord n)
    => (Daemon.InterpState () -> Daemon.DaemonIO (Daemon.InterpState ()))
    -> AppState n
    -> B.EventM n m (AppState n)
runDaemon f appState = do
    interp <- liftIO $ do
        (Daemon.run . f) appState.interpState >>= \case
            Right out -> pure out
            Left er -> error $ show er
    selectPausedLine appState{interpState = interp}

-- | Alternative to 'runDaemon' which returns a value along with the state.
runDaemon2
    :: (Ord n)
    => (Daemon.InterpState () -> Daemon.DaemonIO (Daemon.InterpState (), a))
    -> AppState n
    -> B.EventM n m (AppState n, a)
runDaemon2 f appState = do
    (interp, x) <-
        liftIO $
            (Daemon.run . f) appState.interpState >>= \case
                Right out -> pure out
                Left er -> error $ show er
    newState <- selectPausedLine appState{interpState = interp}
    pure (newState, x)

-- | Determine whether to show the cursor.
handleCursorPosition
    :: AppState AppName
    -- ^ State of the app.
    -> [B.CursorLocation AppName]
    -- ^ Potential Locs
    -> Maybe (B.CursorLocation AppName)
    -- ^ The chosen cursor location if any.
handleCursorPosition s ls =
    if s.activeWindow == AppState.ActiveLiveInterpreter
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
    fl = case selectedFile s of
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
-- Dialog boxes
-- -------------------------------------------------------------------------------------------------

handleDialogQuit :: B.BrickEvent AppName e -> B.EventM AppName (AppState AppName) ()
handleDialogQuit ev = do
    appState <- B.get
    case ev of
        (B.VtyEvent (V.EvKey key _))
            | key == V.KChar 'q' || key == V.KEsc -> do
                B.put $ appState{activeWindow = AppState.ActiveCodeViewport}
            | key == V.KEnter -> quit appState
        _ -> pure ()
    pure ()

handleDialogHelp :: B.BrickEvent AppName e -> B.EventM AppName (AppState AppName) ()
handleDialogHelp (B.VtyEvent (V.EvKey key _))
    | key == V.KChar 'q' || key == V.KEsc || key == V.KEnter = do
        appState <- B.get
        B.put $ appState{activeWindow = AppState.ActiveCodeViewport}
    | key == V.KPageDown = B.vScrollPage scroller B.Down
    | key == V.KPageUp = B.vScrollPage scroller B.Up
    | key == V.KDown = B.vScrollBy scroller 1
    | key == V.KUp = B.vScrollBy scroller (-1)
    | otherwise = pure ()
  where
    scroller = B.viewportScroll HelpViewport
handleDialogHelp _ = pure ()

-- | Stop the TUI.
quit :: AppState n -> B.EventM n s ()
quit appState = liftIO (Daemon.quit appState.interpState) >> B.halt
