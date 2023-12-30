{-# LANGUAGE NamedFieldPuns #-}

module AppState
    ( ActiveWindow (..)
    , AppConfig (..)
    , AppState (..)
    , WidgetSizes
    , changeInfoWidgetSize
    , getInfoWidth
    , getReplHeight
    , changeReplWidgetSize
    , getSelectedModuleInInfoPanel
    , changeSelectedModuleInInfoPanel
    , appInterpState
    , getSourceContents
    , getSourceLineCount
    , filePathOfInfoSelectedModule
    , listAvailableSources
    , liveEditor'
    , makeInitialState
    , resetSelectedLine
    , toggleActiveLineInterpreter
    , toggleBreakpointLine
    , updateSourceMap
    , writeDebugLog
    ) where

import qualified Brick.Widgets.Edit as BE
import Control.Error (atMay, fromMaybe, lastMay)
import Control.Exception (IOException, SomeException, catch, try)
import Control.Monad.IO.Class (MonadIO (..))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Lens.Micro as Lens

import AppConfig (AppConfig (..), resolveStartupSplashPath)
import qualified AppInterpState as AIS
import AppTopLevel (AppName (..))
import Ghcid.Daemon (toggleBreakpointLine)
import qualified Ghcid.Daemon as Daemon
import qualified Loc
import qualified Util

data ActiveWindow
    = ActiveCodeViewport
    | ActiveLiveInterpreter
    | ActiveInfoWindow
    | ActiveDialogQuit
    | ActiveDialogHelp
    deriving (Show, Eq, Ord)

data MaxState = NoMaxState | Maximised | Minimised

-- | Size information of the current GHCiTUI main boxes.
data WidgetSizes = WidgetSizes
    { _wsInfoWidth :: !Int
    , _wsInfoMaxState :: !MaxState
    , _wsReplHeight :: !Int
    , _wsReplMaxState :: !MaxState
    }

{- | Application state wrapper.

Contains information about the UI and configuration. It also holds a
handle to the actual interpreter under the hood, but on the high level
it should not hold anything internal to GHCi or GHCiD.

Prefer to create this with 'makeInitialState'.
-}
data AppState n = AppState
    { interpState :: Daemon.InterpState ()
    -- ^ The interpreter handle.
    , getCurrentWorkingDir :: !FilePath
    -- ^ The current working directory.
    , _appInterpState :: AIS.AppInterpState T.Text n
    -- ^ The live interpreter state (separate from the interpreter
    -- and the app state itself.
    , interpLogs :: ![Text]
    , appConfig :: !AppConfig
    -- ^ Program launch configuration.
    , activeWindow :: !ActiveWindow
    -- ^ Currently active window.
    , selectedFile :: !(Maybe FilePath)
    -- ^ Filepath to the current code viewport contents, if set.
    , selectedLine :: !Int
    -- ^ Currently selected line number. One indexed. Resets back to 1.
    , _infoPanelSelectedModule :: !Int
    -- ^ Currently selected module in the info sidebar, zero indexed.
    , sourceMap :: Map.Map FilePath T.Text
    -- ^ Mapping between source filepaths and their contents.
    , _currentWidgetSizes :: WidgetSizes
    -- ^ Current window/box/panel sizes (since it can change). Do not edit
    -- directly.
    , displayDebugConsoleLogs :: !Bool
    -- ^ Whether to display debug Console logs.
    , debugConsoleLogs :: [Text]
    -- ^ Place for debug output to go.
    , splashContents :: !(Maybe T.Text)
    -- ^ Splash to show on start up.
    }

newtype AppStateM m a = AppStateM {runAppStateM :: m a}

instance (Functor m) => Functor (AppStateM m) where
    fmap f appStateA = AppStateM (f <$> runAppStateM appStateA)

instance (Applicative m) => Applicative (AppStateM m) where
    pure appState = AppStateM (pure appState)
    AppStateM appl <*> AppStateM tgt = AppStateM (appl <*> tgt)

instance (Monad m) => Monad (AppStateM m) where
    return = pure
    AppStateM valM >>= f2 = AppStateM (valM >>= runAppStateM . f2)

instance (MonadIO m) => MonadIO (AppStateM m) where
    liftIO = AppStateM . liftIO

type AppStateIO a = AppStateM IO a

-- | Lens for the App's interpreter box.
appInterpState :: Lens.Lens' (AppState n) (AIS.AppInterpState T.Text n)
appInterpState = Lens.lens _appInterpState (\x ais -> x{_appInterpState = ais})

-- | Lens wrapper for zooming with handleEditorEvent.
liveEditor' :: Lens.Lens' (AppState n) (BE.Editor T.Text n)
liveEditor' = appInterpState . AIS.liveEditor

currentWidgetSizes :: Lens.Lens' (AppState n) WidgetSizes
currentWidgetSizes = Lens.lens _currentWidgetSizes (\x cws -> x{_currentWidgetSizes = cws})

wsInfoWidth :: Lens.Lens' WidgetSizes Int
wsInfoWidth = Lens.lens _wsInfoWidth (\x ipw -> x{_wsInfoWidth = ipw})

wsReplHeight :: Lens.Lens' WidgetSizes Int
wsReplHeight = Lens.lens _wsReplHeight (\x rh -> x{_wsReplHeight = rh})

-- | Write a debug log entry.
writeDebugLog :: T.Text -> AppState n -> AppState n
writeDebugLog lg s = s{debugConsoleLogs = lg : debugConsoleLogs s}

toggleActiveLineInterpreter :: AppState n -> AppState n
toggleActiveLineInterpreter s@AppState{activeWindow} =
    s{activeWindow = toggleLogic activeWindow}
  where
    toggleLogic ActiveLiveInterpreter = ActiveCodeViewport
    toggleLogic _ = ActiveLiveInterpreter

-- | Reset the code viewport selected line to the pause location.
resetSelectedLine :: AppState n -> AppState n
resetSelectedLine s@AppState{interpState} =
    s{selectedFile = ourSelectedFile, selectedLine = ourSelectedLine}
  where
    ourSelectedLine :: Int
    ourSelectedLine =
        fromMaybe
            (selectedLine s)
            (Loc.startLine . Loc.fSourceRange =<< interpState.pauseLoc)
    ourSelectedFile = maybe (selectedFile s) (Just . Loc.filepath) interpState.pauseLoc

-- | Update the source map given any app state changes.
updateSourceMap :: AppState n -> IO (AppState n)
updateSourceMap s = do
    s' <- case selectedFile s of
        Just sf -> updateSourceMapWithFilepath s sf
        Nothing -> pure s
    case s'.interpState.pauseLoc of
        Nothing -> pure s'
        (Just (Loc.FileLoc{filepath})) -> updateSourceMapWithFilepath s' filepath

-- | Update the source map with a given filepath.
updateSourceMapWithFilepath :: AppState n -> FilePath -> IO (AppState n)
updateSourceMapWithFilepath s filepath
    | Map.member filepath s.sourceMap = pure s
    | otherwise = do
        let adjustedFilepath = getCurrentWorkingDir s <> "/" <> filepath
        eContents <- try $ T.readFile adjustedFilepath :: IO (Either IOException T.Text)
        case eContents of
            Left err -> do
                pure $
                    writeDebugLog
                        ( "failed to update source map with "
                            <> T.pack filepath
                            <> ": "
                            <> T.pack (show err)
                        )
                        s
            Right contents -> do
                let newSourceMap = Map.insert filepath contents s.sourceMap
                let logMsg = "updated source map with " <> T.pack filepath
                pure (writeDebugLog logMsg s{sourceMap = newSourceMap})

listAvailableSources :: AppState n -> [(T.Text, FilePath)]
listAvailableSources = Loc.moduleFileMapAssocs . Daemon.moduleFileMap . interpState

-- | Return the potential contents of the current paused file location.
getSourceContents :: AppState n -> Maybe T.Text
getSourceContents s = selectedFile s >>= (sourceMap s Map.!?)

{- | Return the number of lines in the current source viewer.
     Returns Nothing if there's no currently viewed source.
-}
getSourceLineCount :: AppState n -> Maybe Int
getSourceLineCount s = length . T.lines <$> getSourceContents s

changeInfoWidgetSize :: Int -> AppState n -> AppState n
changeInfoWidgetSize amnt s =
    Lens.set
        (currentWidgetSizes . wsInfoWidth)
        -- Do not let the min go too low (<=2), because this causes a memory leak in Brick?
        (Util.clamp (10, 120) (getInfoWidth s + amnt))
        s

changeReplWidgetSize :: Int -> AppState n -> AppState n
changeReplWidgetSize amnt s =
    Lens.set
        (currentWidgetSizes . wsReplHeight)
        -- Do not let the min go too low, because the box disappears then.
        (Util.clamp (1, 80) (getReplHeight s + amnt))
        s

changeSelectedModuleInInfoPanel :: Int -> AppState n -> AppState n
changeSelectedModuleInInfoPanel amnt s =
    s{_infoPanelSelectedModule = _infoPanelSelectedModule s + amnt}

getSelectedModuleInInfoPanel :: AppState n -> Int
getSelectedModuleInInfoPanel = _infoPanelSelectedModule

-- | Return the info box's desired width in character columns.
getInfoWidth :: AppState n -> Int
getInfoWidth = _wsInfoWidth . _currentWidgetSizes

-- | Return the REPL (interactive interpreter)'s box in lines.
getReplHeight :: AppState n -> Int
getReplHeight = _wsReplHeight . _currentWidgetSizes

filePathOfInfoSelectedModule :: AppState n -> Maybe FilePath
filePathOfInfoSelectedModule AppState{interpState, _infoPanelSelectedModule} =
    fmap snd
        . flip atMay _infoPanelSelectedModule
        . Loc.moduleFileMapAssocs
        . Daemon.moduleFileMap
        $ interpState

-- | Initialise the state from the config.
makeInitialState
    :: AppConfig
    -- ^ Start up config.
    -> T.Text
    -- ^ Daemon command prefix.
    -> FilePath
    -- ^ Workding directory.
    -> IO (AppState AppName)
makeInitialState appConfig target cwd = do
    let cwd' = if null cwd then "." else cwd
    let fullCmd = getCmd appConfig <> " " <> target
    interpState <-
        Daemon.run (Daemon.startup (T.unpack fullCmd) cwd') >>= \case
            Right i -> pure i
            Left er -> error (show er)
    splashContents <-
        catch
            (Just <$> (T.readFile =<< resolveStartupSplashPath appConfig))
            -- The splash is never critical.
            -- Just put nothing there if we can't find it.
            (const (pure Nothing) :: SomeException -> IO (Maybe T.Text))
    let selectedFile =
            case Loc.moduleFileMapAssocs (Daemon.moduleFileMap interpState) of
                -- If we just have one file, select that.
                [(_, filepath)] -> Just filepath
                -- If we have no module/file mappings, nothing must be selected.
                [] -> Nothing
                -- If we don't have a selected file, but we have a module loaded,
                -- select the last one.
                xs -> fmap snd (lastMay xs)
    updateSourceMap
        AppState
            { interpState
            , getCurrentWorkingDir = cwd'
            , _appInterpState = AIS.emptyAppInterpState LiveInterpreter
            , activeWindow = ActiveLiveInterpreter
            , appConfig
            , debugConsoleLogs = mempty
            , displayDebugConsoleLogs = getDebugConsoleOnStart appConfig
            , interpLogs = mempty
            , selectedFile
            , selectedLine = 1
            , _infoPanelSelectedModule = 0
            , sourceMap = mempty
            , _currentWidgetSizes =
                WidgetSizes
                    { _wsInfoWidth = 30
                    , _wsInfoMaxState = NoMaxState
                    , _wsReplHeight = 11 -- 10 plus 1 for the entry line.
                    , _wsReplMaxState = NoMaxState
                    }
            , splashContents
            }
