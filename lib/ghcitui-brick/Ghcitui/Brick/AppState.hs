{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Ghcitui.Brick.AppState
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
    , getSourceLineCount
    , selectedFile
    , setSelectedFile
    , selectedLine
    , filePathOfInfoSelectedModule
    , listAvailableSources
    , liveEditor
    , makeInitialState
    , selectPausedLine
    , sourceWindow
    , toggleActiveLineInterpreter
    , toggleBreakpointLine
    , updateSourceMap
    , writeDebugLog
    , daemonReadyToExec
    ) where

import qualified Brick as B
import qualified Brick.BChan as B
import qualified Brick.Widgets.Edit as BE
import Control.Error (atMay, fromMaybe)
import Control.Exception (IOException, try)
import Control.Monad.IO.Class (MonadIO (..))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as Vec
import Lens.Micro ((^.))
import qualified Lens.Micro as Lens

import Ghcitui.Brick.AppConfig (AppConfig (..))
import qualified Ghcitui.Brick.AppConfig as AppConfig
import qualified Ghcitui.Brick.AppInterpState as AIS
import Ghcitui.Brick.AppTopLevel (AppName (..), CustomAppEvent)


import qualified Ghcitui.Brick.SourceWindow as SourceWindow
import Ghcitui.Ghcid.Daemon (toggleBreakpointLine)
import qualified Ghcitui.Ghcid.Daemon as Daemon
import qualified Ghcitui.Ghcid.LogConfig as LogConfig
import qualified Ghcitui.Loc as Loc
import qualified Ghcitui.Util as Util

data ActiveWindow
    = ActiveCodeViewport
    | ActiveLiveInterpreter
    | ActiveInfoWindow
    | ActiveDialogQuit
    | ActiveDialogHelp
    deriving (Show, Eq, Ord)

-- | Size information of the current GHCiTUI main boxes.
data WidgetSizes = WidgetSizes
    { _wsInfoWidth :: !Int
    , _wsReplHeight :: !Int
    }
    deriving (Show)

{- | Application state wrapper.

Contains information about the UI and configuration. It also holds a
handle to the actual interpreter under the hood, but on the high level
it should not hold anything internal to GHCi or GHCiD.

Prefer to create this with 'makeInitialState'.
-}
data AppState n = AppState
    { interpState :: Daemon.InterpState ()
    -- ^ The interpreter handle.
    , eventChannel :: !(B.BChan (CustomAppEvent (AppState n)))
    , waitingOnRepl :: !Bool
    -- ^ Wether we launched a long REPL command that we're waiting on.
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
    , _selectedFile :: !(Maybe FilePath)
    -- ^ Filepath to the current code viewport contents, if set.
    , _sourceWindow :: !(SourceWindow.SourceWindow n T.Text)
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

-- | Lens for the App's interpreter box.
appInterpState :: Lens.Lens' (AppState n) (AIS.AppInterpState T.Text n)
appInterpState = Lens.lens _appInterpState (\x ais -> x{_appInterpState = ais})

-- | Lens wrapper for zooming with handleEditorEvent.
liveEditor :: Lens.Lens' (AppState n) (BE.Editor T.Text n)
liveEditor = appInterpState . AIS.liveEditor

currentWidgetSizes :: Lens.Lens' (AppState n) WidgetSizes
currentWidgetSizes = Lens.lens _currentWidgetSizes (\x cws -> x{_currentWidgetSizes = cws})

wsInfoWidth :: Lens.Lens' WidgetSizes Int
wsInfoWidth = Lens.lens _wsInfoWidth (\x ipw -> x{_wsInfoWidth = ipw})

wsReplHeight :: Lens.Lens' WidgetSizes Int
wsReplHeight = Lens.lens _wsReplHeight (\x rh -> x{_wsReplHeight = rh})

sourceWindow :: Lens.Lens' (AppState n) (SourceWindow.SourceWindow n T.Text)
sourceWindow = Lens.lens _sourceWindow (\x srcW -> x{_sourceWindow = srcW})

selectedFile :: AppState n -> Maybe FilePath
selectedFile = _selectedFile

daemonReadyToExec :: AppState n -> IO Bool
daemonReadyToExec appState = Daemon.readyToExec appState.interpState

-- | Change the selected file for the source window.
setSelectedFile :: (MonadIO m) => Maybe FilePath -> AppState n -> m (AppState n)
setSelectedFile mayFP appState =
    if mayFP == _selectedFile appState
        then do
            -- If we're selecting the same file again, do nothing.
            let debugMsg =
                    "not setting selected file to '"
                        <> maybe "<Nothing>" T.pack mayFP
                        <> "' because it is already set to it"
            pure $ writeDebugLog debugMsg appState
        else do
            -- Update the source map with the new file, and replace the window contents.
            updatedAppState <- liftIO $ updateSourceMap appState{_selectedFile = mayFP}
            let contents = mayFP >>= (sourceMap updatedAppState Map.!?)
            let elements = maybe Vec.empty (Vec.fromList . T.lines) contents
            let newSrcW = SourceWindow.srcWindowReplace elements (appState ^. sourceWindow)
            let debugMsg =
                    "set selected file to '"
                        <> maybe "<Nothing>" T.pack mayFP
                        <> "'"
            pure . writeDebugLog debugMsg $ updatedAppState{_sourceWindow = newSrcW}

-- -------------------------------------------------------------------------------------------------
-- State Line Details
-- -------------------------------------------------------------------------------------------------

-- | Currently selected line number. One-indexed. If no line is selected, returns 1.
selectedLine :: AppState n -> Int
selectedLine s = fromMaybe 1 (s ^. sourceWindow . SourceWindow.srcSelectedLineL)

-- | Reset the code viewport selected line to the pause location.
selectPausedLine :: (Ord n) => AppState n -> B.EventM n m (AppState n)
selectPausedLine s@AppState{interpState} = do
    s' <- setSelectedFile ourSelectedFile s
    let ourSelectedLine :: Int
        ourSelectedLine =
            fromMaybe
                (selectedLine s')
                (Loc.startLine . Loc.fSourceRange =<< interpState.pauseLoc)
    newSrcW <- SourceWindow.setSelectionTo ourSelectedLine (s' ^. sourceWindow)
    pure
        . ( \s'' ->
                writeDebugLog
                    ( "replacing source window. new line: "
                        <> Util.showT (s'' ^. sourceWindow . SourceWindow.srcSelectedLineL)
                        <> " should be "
                        <> Util.showT ourSelectedLine
                        <> ", window start "
                        <> Util.showT (s'' ^. sourceWindow . SourceWindow.srcWindowStartL)
                    )
                    s''
          )
        . Lens.set sourceWindow newSrcW
        $ s'
  where
    ourSelectedFile = maybe (selectedFile s) (Just . Loc.filepath) interpState.pauseLoc

-- | Write a debug log entry.
writeDebugLog :: T.Text -> AppState n -> AppState n
writeDebugLog lg s = s{debugConsoleLogs = take 100 (lg : debugConsoleLogs s)}

toggleActiveLineInterpreter :: AppState n -> AppState n
toggleActiveLineInterpreter s@AppState{activeWindow} =
    s{activeWindow = toggleLogic activeWindow}
  where
    toggleLogic ActiveLiveInterpreter = ActiveCodeViewport
    toggleLogic _ = ActiveLiveInterpreter

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
                let newSourceMap = Map.insert filepath (stripCREndings contents) s.sourceMap
                let logMsg = "updated source map with " <> T.pack filepath
                pure (writeDebugLog logMsg s{sourceMap = newSourceMap})

-- | Remove CR line endings. Important for files checked in on Windows!
stripCREndings :: T.Text -> T.Text
stripCREndings = T.replace "\r" ""

-- | Return the internal mapping of module names to filepaths.
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
    s{_infoPanelSelectedModule = newSelection}
  where
    newSelection = (_infoPanelSelectedModule s + amnt) `mod` numModules
    numModules = length (Loc.moduleFileMapAssocs (Daemon.moduleFileMap (interpState s)))

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
    -- ^ Daemon command suffix.
    -> FilePath
    -- ^ Workding directory.
    -> B.BChan (CustomAppEvent (AppState AppName))
    -- ^ Event channel to handle async with.
    -> IO (AppState AppName)
makeInitialState appConfig target cwd chan = do
    let cwd' = if null cwd then "." else cwd
    let fullCmd = getCmd appConfig <> " " <> target
    let logOutput = case getDebugLogPath appConfig of
            "stderr" -> Daemon.LogOutputStdErr
            "stdout" -> Daemon.LogOutputStdOut
            filepath -> Daemon.LogOutputFile filepath
    let logLevel = LogConfig.LogLevel (AppConfig.getVerbosity appConfig)
    let startupConfig =
            Daemon.StartupConfig
                { Daemon.logLevel = logLevel
                , Daemon.logOutput = logOutput
                }
    interpState <- do
        result <- Daemon.startup (T.unpack fullCmd) cwd' startupConfig
        case result of
            Right iState -> pure iState
            Left er -> error (show er)
    splashContents <- AppConfig.loadStartupSplash appConfig
    let selectedFile' =
            case Loc.moduleFileMapAssocs (Daemon.moduleFileMap interpState) of
                -- If we just have one file, select that.
                [(_, filepath)] -> Just filepath
                -- If we have no module/file mappings, nothing must be selected.
                [] -> Nothing
                -- If we don't have a selected file, but we have a module loaded,
                -- select the last one.
                _ -> Nothing
    setSelectedFile
        selectedFile'
        AppState
            { interpState
            , eventChannel = chan
            , waitingOnRepl = False
            , getCurrentWorkingDir = cwd'
            , _appInterpState = AIS.emptyAppInterpState LiveInterpreter
            , activeWindow = ActiveCodeViewport
            , appConfig
            , debugConsoleLogs = mempty
            , displayDebugConsoleLogs = getDebugConsoleOnStart appConfig
            , interpLogs = mempty
            , _selectedFile = Nothing -- This will be overriden immediately.
            , _infoPanelSelectedModule = 0
            , sourceMap = mempty
            , _currentWidgetSizes =
                WidgetSizes
                    { _wsInfoWidth = 35
                    , _wsReplHeight = 11 -- 10 plus 1 for the entry line.
                    }
            , splashContents
            , _sourceWindow = SourceWindow.mkSourcWindow SourceList ""
            }
