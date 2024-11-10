{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Ghcitui.Brick.Events (handleEvent, handleCursorPosition) where

import qualified Brick.Main as B
import qualified Brick.Types as B
import Control.Monad.IO.Class (MonadIO (..))
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Lens.Micro ((^.))
import qualified Lens.Micro as Lens

import Ghcitui.Brick.AppState as AppState
import Ghcitui.Brick.AppTopLevel
    ( AppName (..)
    , CustomAppEvent (..)
    )
import Ghcitui.Brick.EventUtils (invalidateLineCache)
import qualified Ghcitui.Brick.InterpWindowEvents as InterpWindowEvents
import qualified Ghcitui.Brick.SourceWindow as SourceWindow
import qualified Ghcitui.Brick.SourceWindowEvents as SourceWindowEvents
import qualified Ghcitui.Ghcid.Daemon as Daemon

-- | Handle any Brick event and update the state.
handleEvent :: B.BrickEvent AppName (CustomAppEvent (AppState AppName)) -> B.EventM AppName (AppState AppName) ()
handleEvent (B.VtyEvent (V.EvResize _ _)) = B.invalidateCache
handleEvent (B.VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl])) = do
    -- Handle interrupts right away, regardless of our window.
    appState <- B.get
    liftIO . Daemon.interruptDaemon . AppState.interpState $ appState
    -- Invalidate everything.
    B.invalidateCache
handleEvent (B.AppEvent (ErrorOnCb appState msg)) =
    -- Handle errors cleanly and crash out.
    quit appState >> error (T.unpack msg)
handleEvent (B.AppEvent ev) = do
    SourceWindowEvents.handleSourceWindowPostCb ev
    InterpWindowEvents.handleInterpWindowPostCb ev
handleEvent ev = do
    appState <- B.get
    updatedSourceWindow <- SourceWindow.updateVerticalSpace (appState ^. AppState.sourceWindow)
    let appStateUpdated = Lens.set AppState.sourceWindow updatedSourceWindow appState
    B.put appStateUpdated
    let handler :: B.BrickEvent AppName e -> B.EventM AppName (AppState AppName) ()
        handler = case appStateUpdated.activeWindow of
            AppState.ActiveCodeViewport -> SourceWindowEvents.handleSrcWindowEvent
            AppState.ActiveLiveInterpreter -> InterpWindowEvents.handleInterpreterEvent
            AppState.ActiveInfoWindow -> handleInfoEvent
            AppState.ActiveDialogQuit -> handleDialogQuit
            AppState.ActiveDialogHelp -> handleDialogHelp
    handler ev

-- -------------------------------------------------------------------------------------------------
-- Info Event Handling
-- -------------------------------------------------------------------------------------------------

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

-- -------------------------------------------------------------------------------------------------
-- Handle Cursor Position
-- -------------------------------------------------------------------------------------------------

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
