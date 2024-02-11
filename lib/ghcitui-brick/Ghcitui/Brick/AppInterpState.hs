{-# LANGUAGE RecordWildCards #-}

module Ghcitui.Brick.AppInterpState
    ( AppInterpState (_liveEditor, _viewLock, _commandBuffer, historyPos)
    , commandBuffer
    , emptyAppInterpState
    , futHistoryPos
    , cmdHistory
    , isScanningHist
    , liveEditor
    , pastHistoryPos
    , pushHistory
    , viewLock
    ) where

import qualified Brick.Widgets.Edit as BE
import qualified Data.Text as T
import Lens.Micro as Lens

{- | The state of the Live Interpreter (GHCi) window. The one at the bottom of
     the TUI normally. It's solely visual TUI state. It holds bits of Brick state,
     and only Brick-related things. For example, the last things you ran in the terminal,
     whether you're scrolling the history, and what's the current command buffer.
-}
data AppInterpState s n = AppInterpState
    { _liveEditor :: !(BE.Editor s n)
    -- ^ Brick editor for the actual interactive prompt.
    , _viewLock :: !Bool
    -- ^ Whether we're locked to the bottom of the interpreter (True) window or not (False).
    , _commandBuffer :: ![s]
    -- ^ The text currently typed into the editor, but not yet executed or in the history.
    , _cmdHistory :: ![[s]]
    , historyPos :: !Int
    -- ^ Current position
    }

-- | Lens accessor for the editor. See '_liveEditor'.
liveEditor :: Lens.Lens' (AppInterpState s n) (BE.Editor s n)
liveEditor = Lens.lens _liveEditor (\ais le -> ais{_liveEditor = le})

-- | Lens for the view lock setting. See '_viewLock'.
viewLock :: Lens.Lens' (AppInterpState s n) Bool
viewLock = Lens.lens _viewLock (\ais x -> ais{_viewLock = x})

-- | Lens for the current contents of the command line buffer. See '_commandBuffer'.
commandBuffer :: Lens.Lens' (AppInterpState s n) [s]
commandBuffer = Lens.lens _commandBuffer (\ais x -> ais{_commandBuffer = x})

{- | Return the interpreter command history (what you've typed in the past.)
     Sorted most recent first, oldest last.
-}
cmdHistory :: AppInterpState s n -> [[s]]
cmdHistory = _cmdHistory

-- | Create a base interpreter state.
emptyAppInterpState
    :: n
    -- ^ Name for the 'Brick.Editor'.
    -> AppInterpState T.Text n
emptyAppInterpState name =
    AppInterpState
        { _liveEditor = initInterpWidget name (Just 1)
        , _viewLock = True
        , _commandBuffer = mempty
        , _cmdHistory = mempty
        , historyPos = 0
        }

resetHistoryPos :: AppInterpState s n -> AppInterpState s n
resetHistoryPos s = s{historyPos = 0}

-- | Move interpreter history back.
pastHistoryPos :: AppInterpState s n -> AppInterpState s n
pastHistoryPos s@AppInterpState{..} =
    -- Note we do want it to stop at length _history, not length _history - 1
    -- because 0 is not the beginning of the history, it's the commandBuffer.
    s{historyPos = min (length _cmdHistory) $ succ historyPos}

-- | Are we currently viewing past contents?
isScanningHist :: AppInterpState s n -> Bool
isScanningHist AppInterpState{..} = historyPos /= 0

-- | Move interpreter history forward.
futHistoryPos :: AppInterpState s n -> AppInterpState s n
futHistoryPos s@AppInterpState{..} = s{historyPos = max 0 $ pred historyPos}

-- | Push a new value on to the history stack and reset the position.
pushHistory :: [s] -> AppInterpState s n -> AppInterpState s n
pushHistory cmdLines s = resetHistoryPos $ s{_cmdHistory = cmdLines : cmdHistory s}

-- | Create the initial live interpreter widget object.
initInterpWidget
    :: n
    -- ^ Editor name (must be a unique identifier).
    -> Maybe Int
    -- ^ Line height of the editor. Nothing for unlimited.
    -> BE.Editor T.Text n
initInterpWidget name height = BE.editorText name height mempty
