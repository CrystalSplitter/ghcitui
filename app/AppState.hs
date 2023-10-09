{-# LANGUAGE NamedFieldPuns #-}

module AppState
    ( ActiveWindow (..)
    , AppConfig (..)
    , AppState (..)
    , appInterpState
    , getSourceContents
    , getSourceLineCount
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
import Control.Error (fromMaybe, lastMay)
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

data ActiveWindow = ActiveCodeViewport | ActiveLiveInterpreter | ActiveInfoWindow
    deriving (Show, Eq, Ord)

{- | Size information of the current GHCiDTUI main boxes.
type WindowSizes = [(ActiveWindow, (Maybe Int, Maybe Int))]
-}

-- | Application state wrapper
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
    -- ^ Currently selected line number. Resets back to 1.
    , sourceMap :: Map.Map FilePath T.Text
    -- ^ Mapping between source filepaths and their contents.
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
                pure $ writeDebugLog (T.pack $ show err) s
            Right contents -> do
                let newSourceMap = Map.insert filepath contents s.sourceMap
                pure s{sourceMap = newSourceMap}

listAvailableSources :: AppState n -> [(T.Text, FilePath)]
listAvailableSources = Loc.moduleFileMapAssocs . Daemon.moduleFileMap . interpState

-- | Return the potential contents of the current paused file location.
getSourceContents :: AppState n -> Maybe T.Text
getSourceContents s = s.selectedFile >>= (s.sourceMap Map.!?)

{- | Return the number of lines in the current source viewer.
     Returns Nothing if there's no currently viewed source.
-}
getSourceLineCount :: AppState n -> Maybe Int
getSourceLineCount s = length . T.lines <$> getSourceContents s

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
    interpState <- Daemon.run (Daemon.startup (T.unpack fullCmd) cwd') >>= \case
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
            , sourceMap = mempty
            , splashContents
            }
