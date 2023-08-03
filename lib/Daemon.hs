{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}

module Daemon
    ( startup
    , continue
    , deleteBreakpointLine
    , emptyInterpreterState
    , exec
    , execCleaned
    , execMuted
    , getBpInCurModule
    , getBpInFile
    , load
    , quit
    , setBreakpointLine
    , step
    , stepInto
    , toggleBreakpointLine
    , BreakpointArg (..)
    , InterpState (..)
    ) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (..))
import qualified Data.Bifunctor as Bifunctor
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.String.Interpolate (i)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Language.Haskell.Ghcid as Ghcid

import qualified Loc
import qualified NameBinding
import qualified ParseContext
import Safe

newtype LogLevel = LogLevel Int

data LogOutput = LogOutputStdOut | LogOutputStdErr | LogOutputFile FilePath

data InterpState a = InterpState
    { _ghci :: Ghcid.Ghci
    -- ^ GHCID handle
    , func :: Maybe Text.Text
    -- ^ Current pause position function name.
    , pauseLoc :: Loc.FileLoc
    -- ^ Current pause position.
    , moduleFileMap :: Loc.ModuleFileMap
    -- ^ Mapping between modules and their filepaths.
    , stack :: [String]
    -- ^ Program stack (only available during tracing)
    , breakpoints :: [(Int, Loc.ModuleLoc)]
    -- ^ Currently set breakpoint locations.
    , bindings :: [NameBinding.NameBinding Text.Text]
    -- ^ Current context value bindings.
    , status :: Either Text.Text a
    -- ^ IDK? I had an idea here at one point.
    , logLevel :: LogLevel
    -- ^ How much should we log?
    , logOutput :: LogOutput
    -- ^ Where should we log to?
    , execHist :: [String]
    -- ^ What's the execution history?
    }

instance Show (InterpState a) where
    show s =
        let func' = show s.func
            Loc.FileLoc filepath' lineno' colrange' = s.pauseLoc
         in [i|{func="#{func'}", filepath="#{filepath'}", lineno="#{lineno'}", colrange="#{colrange'}"}|]

emptyInterpreterState :: (Monoid a) => Ghcid.Ghci -> InterpState a
emptyInterpreterState ghci =
    InterpState
        { _ghci = ghci
        , func = Nothing
        , pauseLoc = Loc.FileLoc Nothing Nothing (Nothing, Nothing)
        , moduleFileMap = mempty
        , stack = mempty
        , breakpoints = mempty
        , bindings = mempty
        , status = Right mempty
        , logLevel = LogLevel 3
        , logOutput = LogOutputFile "/tmp/ghcitui.log"
        , execHist = mempty
        }

appendExecHist :: String -> InterpState a -> InterpState a
appendExecHist cmd s@InterpState{execHist} = s{execHist = cmd : execHist}

-- | Start up the GHCI Daemon
startup
    :: String
    -- ^ Command to run (e.g. "ghci" or "cabal repl")
    -> FilePath
    -- ^ Working directory to run the start up command in.
    -> IO (InterpState ())
    -- ^ The newly created interpreter handle.
startup cmd pwd = do
    (ghci, _) <- Ghcid.startGhci cmd (Just pwd) (\_ _ -> pure ())
    pure $ emptyInterpreterState ghci

-- | Shutdown ghcid.
quit :: InterpState a -> IO (InterpState a)
quit state = do
    Ghcid.quit (state._ghci)
    pure state

-- | Update the interpreter state. Wrapper around other updaters.
updateState :: (Monoid a) => InterpState a -> IO (InterpState a)
updateState state =
    updateContext state
        >>= updateBindings
        >>= updateModuleFileMap
        >>= updateBreakList

-- | Update the current interpreter context.
updateContext :: (Monoid a) => InterpState a -> IO (InterpState a)
updateContext state@InterpState{_ghci} = do
    logDebug state "|updateContext| CMD: :show context\n"
    msgs <- Ghcid.exec _ghci ":show context"
    let feedback = ParseContext.cleanResponse msgs
    logDebug
        state
        ( "|updateContext| OUT:\n"
            `Text.append` ParseContext.linesToText msgs
            `Text.append` "\n"
        )
    if Text.null feedback
        then pure (emptyInterpreterState _ghci) -- We exited everything.
        else do
            let ctx = ParseContext.parseContext feedback
            pure
                state
                    { func = ctx.func
                    , pauseLoc = Loc.FileLoc ctx.filepath ctx.lineno ctx.colrange
                    }

-- | Update the current local bindings.
updateBindings :: InterpState a -> IO (InterpState a)
updateBindings state@InterpState{_ghci} = do
    logDebug state "|updateBindings| CMD: :show bindings\n"
    msgs <- Ghcid.exec _ghci ":show bindings"
    let feedback = ParseContext.cleanResponse msgs
    logDebug
        state
        ( "|updateBindings| OUT:\n"
            `Text.append` ParseContext.linesToText msgs
            `Text.append` "\n"
        )
    case ParseContext.parseBindings feedback of
        Right bindings -> pure (state{bindings})
        _ -> error "Failed to updateBindings"

-- | Update the source map given any app state changes.
updateModuleFileMap :: InterpState a -> IO (InterpState a)
updateModuleFileMap state@InterpState{_ghci} = do
    modules <- Ghcid.showModules _ghci
    let moduleFileMap = Loc.ModuleFileMap $ Bifunctor.first Text.pack <$> modules
    pure $ state{moduleFileMap}

-- | Analogue to ":step".
step :: (Monoid a) => InterpState a -> IO (InterpState a)
step state = execMuted state ":step"

-- | Analogue to ":step <func>".
stepInto
    :: (Monoid a)
    => InterpState a
    -> String
    -- ^ Function name to jump to
    -> IO (InterpState a)
    -- ^ New interpreter state
stepInto state func = execMuted state (":step " ++ func)

-- | Analogue to ":continue". Throws out any messages.
continue :: (Monoid a) => InterpState a -> IO (InterpState a)
continue state = execMuted state ":continue"

-- | Analogue to ":load <filepath>". Throws out any messages.
load :: (Monoid a) => InterpState a -> FilePath -> IO (InterpState a)
load state filepath = execMuted state (":l " ++ filepath)

-- | Execute an arbitrary command, as if it was directly written in GHCi.
exec :: (Monoid a) => InterpState a -> String -> IO (InterpState a, [String])
exec state@InterpState{_ghci} cmd = do
    logDebug state ("|exec| CMD: " `Text.append` Text.pack cmd)
    msgs <- Ghcid.exec _ghci cmd
    logDebug
        state
        ( "|exec| OUT:\n"
            `Text.append` ParseContext.linesToText msgs
            `Text.append` "\n"
        )
    newState <- updateState $ appendExecHist cmd state
    pure (newState, msgs)

-- | @exec@, but throw out any messages.
execMuted :: (Monoid a) => InterpState a -> String -> IO (InterpState a)
execMuted state cmd = do
    (newState, _) <- exec state cmd
    pure newState

-- | @exec@, but clean the message from prompt.
execCleaned :: (Monoid a) => InterpState a -> String -> IO (InterpState a, [String])
execCleaned state cmd = do
    res <- cleaner <$> exec state cmd
    logDebug state ("|cleaned|:\n" `Text.append` (Text.pack . unlines . snd $ res))
    pure res
  where
    cleaner (s, ls) = (s, Text.unpack <$> (Text.lines . ParseContext.cleanResponse) ls)

-- | Location info passed to *BreakpointLine functions.
data BreakpointArg
    = -- | Location in the current file.
      LocalLine !Int
    | -- | Location in a module.
      ModLoc Loc.ModuleLoc
    deriving (Show, Eq, Ord)

-- | Toggle a breakpoint
toggleBreakpointLine :: (Monoid a) => InterpState a -> BreakpointArg -> IO (InterpState a)
toggleBreakpointLine state loc
    | Right True <- isSet = deleteBreakpointLine state loc
    | Left x <- isSet = error x
    | otherwise = setBreakpointLine state loc
  where
    invalidLoc = Left "Cannot locate breakpoint position in module without source"
    handleModLoc ml =
        let
            fileLoc = maybe invalidLoc Right (Loc.toFileLoc state.moduleFileMap ml)
         in
            fileLoc >>= \fl -> case (fl.linenoF, fl.filepath) of
                (Just lineno, Just filepath) ->
                    Right $ lineno `elem` getBpInFile state filepath
                (_, _) -> invalidLoc
    isSet =
        case loc of
            LocalLine lineno -> Right $ lineno `elem` getBpInCurModule state
            ModLoc ml -> handleModLoc ml

-- | Set a breakpoint at a given line.
setBreakpointLine :: (Monoid a) => InterpState a -> BreakpointArg -> IO (InterpState a)
setBreakpointLine state loc = execMuted state command
  where
    command =
        ":break " ++ case loc of
            LocalLine pos -> show pos
            ModLoc (Loc.ModuleLoc modM posM (colnoM, _)) ->
                let mod' = Text.unpack $ fromMaybe "" modM
                    pos = maybe "" show posM
                    colno = maybe "" show colnoM
                 in if pos == ""
                        then error "Cannot set breakpoint at unknown line number"
                        else mod' ++ " " ++ pos ++ " " ++ colno

-- | Delete a breakpoint at a given line.
deleteBreakpointLine :: (Monoid a) => InterpState a -> BreakpointArg -> IO (InterpState a)
deleteBreakpointLine state loc =
    let convert (LocalLine ll) =
            -- TODO: We really should not consider LocalLines valid for this, because we don't
            -- really know whether it's local to the paused file, or local to the file
            -- we're viewing.
            -- But that's a problem for future me.
            Loc.toModuleLoc state.moduleFileMap (state.pauseLoc{Loc.linenoF = Just ll})
        convert (ModLoc ml) = Just ml

        -- Get the breakpoint index if it exists.
        idxMaybe =
            convert loc >>= \Loc.ModuleLoc{modName, linenoM} ->
                headMay
                    [ idx
                    | (idx, otherML) <- state.breakpoints
                    , otherML.modName == modName && otherML.linenoM == linenoM
                    ]
     in case idxMaybe of
            Just num -> execMuted state (":delete " ++ show num)
            Nothing -> do
                logDebug
                    state
                    ( Text.pack $
                        "No breakpoint at "
                            ++ show loc
                            ++ "; breakpoints are found at "
                            ++ show state.breakpoints
                    )
                pure state

updateBreakList :: InterpState a -> IO (InterpState a)
updateBreakList state@InterpState{_ghci} = do
    logDebug state "|updateBreakList| CMD: :show breaks\n"
    msgs <- Ghcid.exec _ghci ":show breaks"
    logDebug
        state
        ( "|updateBreakList| OUT:\n"
            `Text.append` ParseContext.linesToText msgs
        )
    let response = ParseContext.cleanResponse msgs
    pure
        ( case ParseContext.parseShowBreaks response of
            Right breakpoints -> state{breakpoints}
            Left err -> error ("parsing breakpoint list: " ++ Text.unpack err)
        )

-- | Return a list of breakpoint line numbers in the current file.
getBpInCurModule :: InterpState a -> [Int]
getBpInCurModule InterpState{pauseLoc = Loc.FileLoc{filepath = Nothing}} = []
getBpInCurModule s@InterpState{pauseLoc = Loc.FileLoc{filepath = Just fp}} = getBpInFile s fp

-- | Return a list of breakpoint line numbers in the given filepath.
getBpInFile :: InterpState a -> FilePath -> [Int]
getBpInFile s fp = catMaybes [loc.linenoF | loc <- breakpointlocs, loc.filepath == Just fp]
  where
    -- Convert between module locations and file locations
    convert = Loc.toFileLoc s.moduleFileMap . snd
    breakpointlocs = mapMaybe convert s.breakpoints

-- | Log a message at the Debug level.
logDebug :: (MonadIO m) => InterpState a -> Text.Text -> m ()
logDebug s msg =
    liftIO $ do
        when (num >= 3) $
            logHelper output "[DEBUG]: " msg
  where
    LogLevel num = s.logLevel
    output = logOutput s

logHelper
    :: (MonadIO m)
    => LogOutput
    -- ^ Where to log?
    -> Text.Text
    -- ^ prefix
    -> Text.Text
    -- ^ Message
    -> m ()
logHelper outputLoc prefix msg = do
    liftIO $ case outputLoc of
        LogOutputFile path -> Text.appendFile path fmtMsg
        LogOutputStdOut -> Text.putStrLn fmtMsg
        _ -> error "Cannot log to that output configuration."
  where
    fmtMsg = Text.unlines [prefix `Text.append` line | line <- Text.lines msg]
