{-# LANGUAGE NamedFieldPuns #-}

module AppState
    ( ActiveWindow (..)
    , AppConfig (..)
    , AppState (..)
    , getSourceContents
    , updateSourceMap
    , resetSelectedLine
    , makeInitialState
    , toggleActiveLineInterpreter
    , toggleBreakpointLine
    , appInterpState
    , liveEditor'
    , writeDebugLog
    , WindowSizes
    ) where

import AppConfig (AppConfig (..), resolveStartupSplashPath)
import qualified AppInterpState as AIS
import qualified Brick.Widgets.Edit as BE
import Control.Exception (SomeException, catch)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text, unpack)
import qualified Data.Text.IO
import qualified Lens.Micro as Lens

import AppTopLevel (AppName (..))
import Ghcid.Daemon (toggleBreakpointLine)
import qualified Ghcid.Daemon as Daemon
import qualified Loc

data ActiveWindow = ActiveCodeViewport | ActiveLiveInterpreter | ActiveInfoWindow
    deriving (Show, Eq, Ord)

-- | Size information of the current GHCiDTUI main boxes.
type WindowSizes = [(ActiveWindow, (Maybe Int, Maybe Int))]

-- | Application state wrapper
data AppState n = AppState
    { interpState :: Daemon.InterpState ()
    -- ^ The interpreter handle.
    , _appInterpState :: AIS.AppInterpState Text n
    -- ^ The live interpreter state (separate from the interpreter
    -- and the app state itself.
    , interpLogs :: [Text]
    , appConfig :: AppConfig
    -- ^ Program launch configuration.
    , activeWindow :: ActiveWindow
    -- ^ Currently active window.
    , selectedFile :: Maybe FilePath
    -- ^ Filepath to the current code viewport contents, if set.
    , selectedLine :: Int
    -- ^ Currently selected line number. Resets back to 1.
    , sourceMap :: Map.Map FilePath Text
    -- ^ Mapping between source filepaths and their contents.
    , displayDebugConsoleLogs :: Bool
    -- ^ Whether to display debug Console logs.
    , debugConsoleLogs :: [Text]
    -- ^ Place for debug output to go.
    , splashContents :: !(Maybe Text)
    -- ^ Splash to show on start up.
    }

-- | Lens for the App's interpreter box.
appInterpState :: Lens.Lens' (AppState n) (AIS.AppInterpState Text n)
appInterpState = Lens.lens _appInterpState (\x ais -> x{_appInterpState = ais})

-- | Lens wrapper for zooming with handleEditorEvent.
liveEditor' :: Lens.Lens' (AppState n) (BE.Editor Text n)
liveEditor' = appInterpState . AIS.liveEditor

-- | Write a debug log entry.
writeDebugLog :: Text -> AppState n -> AppState n
writeDebugLog lg s = s{debugConsoleLogs = lg : debugConsoleLogs s}

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
        pure s{sourceMap = newSourceMap}

-- | Return the potential contents of the current paused file location.
getSourceContents :: AppState n -> Maybe Text
getSourceContents s = s.selectedFile >>= (s.sourceMap Map.!?)

-- | Initialise the state from the config.
makeInitialState
    :: AppConfig
    -- ^ Start up config.
    -> Text
    -- ^ Daemon command prefix.
    -> FilePath
    -- ^ Workding directory.
    -> IO (AppState AppName)
makeInitialState appConfig target cwd = do
    let cwd' = if null cwd then "." else cwd
    let fullCmd = getCmd appConfig <> " " <> target
    interpState <- Daemon.startup (unpack fullCmd) cwd'
    splashContents <-
        catch
            (Just <$> (Data.Text.IO.readFile =<< resolveStartupSplashPath appConfig))
            -- The splash is never critical.
            -- Just put nothing there if we can't find it.
            (const (pure Nothing) :: SomeException -> IO (Maybe Text))
    pure
        AppState
            { interpState
            , _appInterpState = AIS.emptyAppInterpState LiveInterpreter
            , activeWindow = ActiveCodeViewport
            , appConfig
            , debugConsoleLogs = mempty
            , displayDebugConsoleLogs = getDebugConsoleOnStart appConfig
            , interpLogs = mempty
            , selectedFile = Nothing
            , selectedLine = 1
            , sourceMap = mempty
            , splashContents
            }
