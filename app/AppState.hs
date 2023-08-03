{-# LANGUAGE NamedFieldPuns #-}

module AppState
    ( ActiveWindow (..)
    , AppConfig (..)
    , AppState (..)
    , getSourceContents
    , updateSourceMap
    , liveEditorLens
    , resetSelectedLine
    , makeInitialState
    , AppStateConfig (..)
    , toggleActiveLineInterpreter
    , toggleBreakpointLine
    ) where

import qualified Brick.Widgets.Edit as BE
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text.IO
import qualified Lens.Micro as Lens

import AppTopLevel (AppName (..), Command)
import Daemon (toggleBreakpointLine)
import qualified Daemon
import qualified Loc

data ActiveWindow = ActiveCodeViewport | ActiveLiveInterpreter | ActiveInfoWindow
    deriving (Show, Eq, Ord)

newtype AppConfig = AppConfig
    { interpreterPrompt :: Text
    -- ^ Prompt to show for the live interpreter.
    }
    deriving (Eq, Show)

-- | Application state wrapper
data AppState n = AppState
    { interpState :: Daemon.InterpState ()
    , liveEditor :: BE.Editor Text n
    , interpLogs :: [Text]
    , appConfig :: AppConfig
    -- ^ The interpreter handle.
    , activeWindow :: ActiveWindow
    -- ^ Currently active interpreter.
    , selectedFile :: Maybe FilePath
    -- ^ Filepath to the current code viewport contents, if set.
    , selectedLine :: Int
    -- ^ Currently selected line number. Resets back to 1.
    , sourceMap :: Map.Map FilePath Text
    -- ^ Mapping between source filepaths and their contents.
    , appStateConfig :: AppStateConfig
    -- ^ Program launch configuration.
    , displayDebugConsoleLogs :: Bool
    -- ^ Whether to display debug Console logs.
    , debugConsoleLogs :: [Text]
    -- ^ Place for debug output to go.
    , splashContents :: !(Maybe Text)
    -- ^ Splash to show on start up.
    , liveInterpreterViewLock :: !Bool
    -- ^ Lock the live interpreter view to the bottom.
    }

-- | Lens wrapper for zooming with handleEditorEvent.
liveEditorLens :: Lens.Lens' (AppState n) (BE.Editor Text n)
liveEditorLens = Lens.lens liveEditor (\s newEditor -> s{liveEditor = newEditor})

toggleActiveLineInterpreter :: AppState n -> AppState n
toggleActiveLineInterpreter s@AppState{activeWindow} =
    s{activeWindow = toggleLogic activeWindow}
  where
    toggleLogic ActiveLiveInterpreter = ActiveCodeViewport
    toggleLogic _ = ActiveLiveInterpreter

-- | Reset the code viewport selected line.
resetSelectedLine :: AppState n -> AppState n
resetSelectedLine s@AppState{interpState} = s{selectedFile, selectedLine}
  where
    selectedLine = fromMaybe 1 interpState.pauseLoc.linenoF
    selectedFile = interpState.pauseLoc.filepath

-- | Update the source map given any app state changes.
updateSourceMap :: AppState n -> IO (AppState n)
updateSourceMap s =
    case s.interpState.pauseLoc.filepath of
        Nothing -> pure s
        (Just filepath) -> updateSourceMapWithFilepath s filepath

-- | Update the source map with a given filepath.
updateSourceMapWithFilepath :: AppState n -> FilePath -> IO (AppState n)
updateSourceMapWithFilepath s filepath
    | Map.member filepath s.sourceMap = pure s
    | otherwise = do
        contents <- Data.Text.IO.readFile filepath
        let newSourceMap = Map.insert filepath contents s.sourceMap
        pure $ s{sourceMap = newSourceMap}

-- | Return the potential contents of the current paused file location.
getSourceContents :: AppState n -> Maybe Text
getSourceContents s = s.selectedFile >>= (s.sourceMap Map.!?)

-- | Configuration options read in at start-up.
newtype AppStateConfig = AppStateConfig
    { startupSplashPath :: FilePath
    }

-- | Create the initial live interpreter widget object.
initInterpWidget
    :: n
    -- ^ Editor name (must be a unique identifier).
    -> Maybe Int
    -- ^ Line height of the editor. Nothing for unlimited.
    -> BE.Editor Text n
initInterpWidget name height = BE.editorText name height ""

-- TODO: This should not be hardcoded for debugging.
makeInitialState :: AppStateConfig -> Command -> IO (AppState AppName)
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
            , appConfig =
                AppConfig
                    { interpreterPrompt = "ghci> "
                    }
            , activeWindow = ActiveCodeViewport
            , liveEditor = initInterpWidget LiveInterpreter (Just 1)
            , interpLogs = []
            , displayDebugConsoleLogs = False
            , debugConsoleLogs = mempty
            , splashContents = Just splashContents
            , liveInterpreterViewLock = True
            }
