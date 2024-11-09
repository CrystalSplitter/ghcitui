{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Ghcitui.Brick.InterpWindowEvents where

import qualified Brick.Main as B
import qualified Brick.Types as B
import qualified Brick.Widgets.Edit as BE
import Control.Error (atDef, lastDef)
import Control.Monad.IO.Class (MonadIO (..))
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
import Ghcitui.Brick.EventUtils
    ( commonPrefixes
    , runDaemon2
    , reflowText
    )
import qualified Ghcitui.Ghcid.Daemon as Daemon
import Ghcitui.Util (showT)

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
            let newAppState2 =
                    writeDebugLog ("handled Enter: Ran '" <> cmd <> "'")
                        . Lens.set (appInterpState . AIS.viewLock) True
                        . Lens.over appInterpState (AIS.pushHistory [cmd])
                        $ appendToLogs output cmd newAppState1
            let appStateFinalIO = updateSourceMap (Lens.set liveEditor newEditor newAppState2)
            B.put =<< liftIO appStateFinalIO
            -- Invalidate the entire render state of the application
            -- because we don't know what's actually changed here now.
            B.invalidateCache
        B.VtyEvent (V.EvKey (V.KChar '\t') []) -> do
            -- We want to preserve spaces, but not trailing newlines.
            let cmd = T.dropWhileEnd ('\n' ==) . T.unlines . editorContents $ appState
            -- Tab completion expects input to be 'show'n in quotes.
            -- There's probably a better way of doing this!
            (newAppState, (prefix, completions)) <- runDaemon2 (Daemon.tabComplete cmd) appState
            let maxCompletionLen = maximum $ T.length <$> completions
            let columnPadding = 1
            extent <-
                B.lookupExtent LiveInterpreterViewport >>= \case
                    Just e -> pure e
                    Nothing -> error "Could not find extent of LiveInterpreterViewport"
            let interpWidth = fst . B.extentSize $ extent
            let completionColWidth = min (interpWidth - 2) maxCompletionLen + columnPadding
            let numCols = interpWidth `div` completionColWidth
            let updateCompletions cs s = case cs of
                    -- Only one completion, just replace the entire buffer with it.
                    [c] -> replaceCommandBuffer (prefix <> c <> " ") s
                    -- No completions. Just go to a new prompt.
                    [] -> appendToLogs [] cmd s
                    -- Replace the buffer with the longest possible prefix among options, and
                    -- print the remaining.
                    _ ->
                        replaceCommandBuffer (prefix <> commonPrefixes cs)
                            . appendToLogs (reflowText numCols completionColWidth cs) cmd
                            $ s
            B.put
                . writeDebugLog
                    ( "handled Tab, Prefix was: '"
                        <> cmd
                        <> "' completions were: "
                        <> showT completions
                    )
                . updateCompletions completions
                $ newAppState
        B.VtyEvent (V.EvKey (V.KChar 'x') [V.MCtrl]) ->
            -- Toggle out of the interpreter.
            leaveInterpreter
        B.VtyEvent (V.EvKey V.KEsc _) -> do
            if not $ appState ^. appInterpState . AIS.viewLock
                then -- Exit scroll mode first.
                    B.put (Lens.set (appInterpState . AIS.viewLock) True appState)
                else -- Also toggle out of the interpreter.
                    leaveInterpreter

        -- Selecting previous commands.
        B.VtyEvent (V.EvKey V.KUp _) -> do
            let maybeStoreBuffer s =
                    if not (AIS.isScanningHist (getAis s))
                        then storeCommandBuffer s
                        else s
            let wDebug s =
                    writeDebugLog
                        ( "handled Up; historyPos is "
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
                        ( "handled Down; historyPos is "
                            <> (showT . AIS.historyPos . getAis $ s)
                        )
                        s
            let appState' =
                    wDebug
                        . replaceCommandBufferWithHist -- Display the history.
                        . Lens.over appInterpState AIS.futHistoryPos -- Go forward in time.
                        $ appState
            B.put appState'

        -- Scrolling back through the logs.
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

appendToLogs
    :: [T.Text]
    -- ^ Logs between commands.
    -> T.Text
    -- ^ The command sent to produce the logs.
    -> AppState n
    -- ^ State to update.
    -> AppState n
    -- ^ Updated state.
appendToLogs logs promptEntry state = state{interpLogs = take interpreterLogLimit combinedLogs}
  where
    combinedLogs = reverse logs <> (formattedWithPrompt : interpLogs state)
    formattedWithPrompt = getInterpreterPrompt (appConfig state) <> promptEntry
    -- TODO: Should be configurable?
    interpreterLogLimit = 1000

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