module AppInterpState (
    AppInterpState (..),
    commandBuffer,
    emptyAppInterpState,
    futHistoryPos,
    history,
    isScanningHist,
    liveEditor,
    pastHistoryPos,
    pushHistory,
    viewLock,
) where

import qualified Brick.Widgets.Edit as BE
import Data.Text (Text)
import Lens.Micro as Lens

data AppInterpState s n = AppInterpState
    { _liveEditor :: BE.Editor s n
    , _viewLock :: !Bool
    , _commandBuffer :: [s]
    , _history :: [[s]]
    , historyPos :: !Int
    }

-- | Lens accessor for the editor.
liveEditor :: Lens.Lens' (AppInterpState s n) (BE.Editor s n)
liveEditor = Lens.lens _liveEditor (\ais le -> ais { _liveEditor = le })

viewLock :: Lens.Lens' (AppInterpState s n) Bool
viewLock = Lens.lens _viewLock (\ais x -> ais { _viewLock = x })

commandBuffer :: Lens.Lens' (AppInterpState s n) [s]
commandBuffer = Lens.lens _commandBuffer (\ais x -> ais { _commandBuffer = x })

history :: AppInterpState s n -> [[s]]
history = _history

emptyAppInterpState :: n -> AppInterpState Text n
emptyAppInterpState name =
    AppInterpState
        { _liveEditor = initInterpWidget name (Just 1)
        , _viewLock = True
        , _commandBuffer = mempty
        , _history = mempty
        , historyPos = 0
        }

-- | Move interpreter history back.
pastHistoryPos :: AppInterpState s n -> AppInterpState s n
pastHistoryPos s@AppInterpState{..} =
    -- Note we do want it to stop at length _history, not length _history - 1
    -- because 0 is not the beginning of the history, it's the commandBuffer.
    s{historyPos = min (length _history) $ succ historyPos}

-- | Are we currently viewing past contents?
isScanningHist :: AppInterpState s n -> Bool
isScanningHist AppInterpState{..} = historyPos /= 0

-- | Move interpreter history forward.
futHistoryPos :: AppInterpState s n -> AppInterpState s n
futHistoryPos s@AppInterpState{..} = s{historyPos = max 0 $ pred historyPos}

-- | Push a new value on to the history stack.
pushHistory :: [s] -> AppInterpState s n -> AppInterpState s n
pushHistory cmdLines s = s {_history = cmdLines : history s}

-- | Create the initial live interpreter widget object.
initInterpWidget
    :: n
    -- ^ Editor name (must be a unique identifier).
    -> Maybe Int
    -- ^ Line height of the editor. Nothing for unlimited.
    -> BE.Editor Text n
initInterpWidget name height = BE.editorText name height mempty
