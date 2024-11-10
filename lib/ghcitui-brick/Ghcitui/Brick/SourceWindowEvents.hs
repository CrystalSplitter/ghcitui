{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Ghcitui.Brick.SourceWindowEvents (handleSrcWindowEvent, handleSourceWindowPostCb) where

import qualified Brick.Main as B
import qualified Brick.Types as B
import Control.Error (fromMaybe, note)
import Control.Monad.IO.Class (MonadIO (..))
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Lens.Micro ((^.))
import qualified Lens.Micro as Lens

import qualified Brick.BChan as B
import Ghcitui.Brick.AppState as AppState
import Ghcitui.Brick.AppTopLevel
    ( AppName (..)
    , CustomAppEvent (..)
    )
import Ghcitui.Brick.EventUtils
import qualified Ghcitui.Brick.SourceWindow as SourceWindow
import qualified Ghcitui.Ghcid.Daemon as Daemon
import qualified Ghcitui.Loc as Loc
import Ghcitui.Util (showT)

-- -------------------------------------------------------------------------------------------------
-- Code Viewport Event Handling
-- -------------------------------------------------------------------------------------------------

-- TODO: Handle mouse events?
handleSrcWindowEvent :: B.BrickEvent AppName e -> B.EventM AppName (AppState AppName) ()
handleSrcWindowEvent (B.VtyEvent (V.EvKey key ms))
    | key `elem` [V.KChar 'q', V.KEsc] = do
        confirmQuit

    -- GHCi Blocking Events.
    | key == V.KChar 's' = do
        appState@AppState.AppState{AppState.interpState} <- B.get
        let callback = stepCb appState
        liftIO $ Daemon.scheduleWithCb interpState (Daemon.step interpState) callback
    | key == V.KChar 'c' = do
        appState@AppState.AppState{AppState.interpState} <- B.get
        let callback = stepCb appState
        liftIO $ Daemon.scheduleWithCb interpState (Daemon.step interpState) callback
    | key == V.KChar 't' = do
        appState@AppState.AppState{AppState.interpState} <- B.get
        let callback = stepCb appState
        liftIO $ Daemon.scheduleWithCb interpState (Daemon.trace interpState) callback
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

{- | Redraw Step Callback. Called asynchronously after the 'DaemonIO' resolves
     for 'step' and similar.
-}
stepCb
    :: AppState n
    -- ^ 'AppState' to use for asynchronous channel communication.
    -> Either Daemon.DaemonError (Daemon.InterpState ())
    -- ^ The incoming response from the Daemon for the 'step' (or similar) operation.
    -> IO ()
    -- ^ IO used to write to the event bounded channel.
stepCb appState (Right interpState) =
    B.writeBChan (AppState.eventChannel appState) (StepCb appState{interpState})
stepCb appState (Left msg) =
    B.writeBChan (AppState.eventChannel appState) (ErrorOnCb appState (showT msg))

breakpointCb
    :: Loc.ModuleLoc
    -> AppState n
    -> Either Daemon.DaemonError (Daemon.InterpState ())
    -> IO ()
breakpointCb moduleLoc appState (Right interpState) =
    B.writeBChan
        (AppState.eventChannel appState)
        (BreakpointCb appState{interpState} moduleLoc)
breakpointCb _ appState (Left msg) =
    B.writeBChan (AppState.eventChannel appState) (ErrorOnCb appState (showT msg))

-- | Synchronous code to update the state after a SourceWindowEvent callback.
handleSourceWindowPostCb
    :: CustomAppEvent (AppState AppName) -> B.EventM AppName (AppState AppName) ()
handleSourceWindowPostCb (StepCb appState) = do
    B.put =<< AppState.selectPausedLine appState
    invalidateLineCache
handleSourceWindowPostCb (BreakpointCb appState moduleLoc) = do
    let interpState = AppState.interpState appState
    -- We may need to be smarter about this,
    -- because there's a chance that the module loc 'ml'
    -- doesn't actually refer to this viewed file?
    case Loc.singleify (Loc.sourceRange moduleLoc) of
        Just (lineno, _colrange) ->
            invalidateCachedLine lineno
        _ ->
            -- If we don't know, just invalidate everything.
            invalidateLineCache
    B.put appState{interpState}
-- For all other AppEvent types, ignore them. They're handled elsewhere.
handleSourceWindowPostCb _ = pure ()

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
    B.put $ writeDebugLog ("selected line is: " <> showT newLineno) movedAppState

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
            let interpState = AppState.interpState appState
            let daemonOp = Daemon.toggleBreakpointLine (Daemon.ModLoc ml) interpState
            let callback = breakpointCb ml appState
            liftIO $
                Daemon.scheduleWithCb
                    interpState
                    daemonOp
                    callback

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
