{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module Ghcitui.Ghcid.Daemon
    ( -- * The interpreter state
      InterpState
        ( func
        , pauseLoc
        , moduleFileMap
        , breakpoints
        , bindings
        , logLevel
        , logOutput
        , execHist
        , traceHist
        )
    , emptyInterpreterState

      -- * Startup and shutdown
    , startup
    , StartupConfig (..)
    , quit

      -- * Base operations with the daemon
    , exec
    , execCleaned
    , execMuted

      -- * Wrapped operations with the daemon
    , step
    , stepInto
    , load
    , continue

      -- * Breakpoints
    , getBpInCurModule
    , getBpInFile
    , toggleBreakpointLine
    , setBreakpointLine
    , deleteBreakpointLine

      -- * Tracing
    , trace
    , history

      -- * Tab completion
    , tabComplete

      -- * Misc
    , isExecuting
    , BreakpointArg (..)
    , run
    , schedule
    , scheduleWithCb
    , interruptDaemon
    , DaemonIO
    , DaemonError
    , LogOutput (..)
    ) where

import Control.Concurrent (MVar, forkIO, newEmptyMVar, newMVar, putMVar, takeMVar)
import Control.Error
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.String.Interpolate (i)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Language.Haskell.Ghcid as Ghcid
import qualified System.IO as IO

import Ghcitui.Ghcid.LogConfig (LogLevel (..), LogOutput (..))
import qualified Ghcitui.Ghcid.ParseContext as ParseContext
import qualified Ghcitui.Ghcid.ParseTabCompletions as ParseTabCompletions
import Ghcitui.Ghcid.StartupConfig (StartupConfig)
import qualified Ghcitui.Ghcid.StartupConfig as StartupConfig
import qualified Ghcitui.Loc as Loc
import qualified Ghcitui.NameBinding as NameBinding
import Ghcitui.Util (showT)
import qualified Ghcitui.Util as Util

type GhciHandle = Ghcid.Ghci

data InterpState a = InterpState
    { _ghci :: !GhciHandle
    -- ^ GHCiD handle.
    , _ghciLock :: !(MVar ())
    -- ^ Lock for single-threaded GHCi operations.
    , func :: !(Maybe T.Text)
    -- ^ Current pause position function name.
    , pauseLoc :: !(Maybe Loc.FileLoc)
    -- ^ Current pause position.
    , moduleFileMap :: !Loc.ModuleFileMap
    -- ^ Mapping between modules and their filepaths.
    , stack :: ![T.Text]
    -- ^ Program stack (only available during tracing).
    , breakpoints :: ![(Int, Loc.ModuleLoc)]
    -- ^ Currently set breakpoint locations.
    , bindings :: !(Either DaemonError [NameBinding.NameBinding T.Text])
    -- ^ Current context value bindings.
    , status :: !(Either T.Text a)
    -- ^ IDK? I had an idea here at one point.
    , logLevel :: !LogLevel
    -- ^ How much should we log?
    , logOutput :: !LogOutput
    -- ^ Where should we log to?
    , execHist :: ![T.Text]
    -- ^ What's the execution history? Note: different from trace history.
    , traceHist :: ![T.Text]
    -- ^ Trace history.
    }

instance Show (InterpState a) where
    show s =
        let func' = show s.func
            msg = case s.pauseLoc of
                Just (Loc.FileLoc filepath' Loc.SourceRange{..}) ->
                    let srcRngFmt :: String
                        srcRngFmt =
                            [i|{sourceRange=(#{startLine},#{startCol})-(#{endLine},#{endCol})}|]
                     in [i|{func=#{func'}, filepath=#{filepath'}, #{srcRngFmt}}|]
                Nothing -> "<unknown pause location>" :: String
         in msg

{- | Create an empty/starting interpreter state.
     Usually you don't want to call this directly. Instead use 'startup'.
-}
emptyInterpreterState :: (Monoid a) => GhciHandle -> StartupConfig -> IO (InterpState a)
emptyInterpreterState ghci startupConfig = do
    ghciLock <- newMVar ()
    pure $
        InterpState
            { _ghci = ghci
            , _ghciLock = ghciLock
            , func = Nothing
            , pauseLoc = Nothing
            , moduleFileMap = mempty
            , stack = mempty
            , breakpoints = mempty
            , bindings = Right mempty
            , status = Right mempty
            , logLevel = StartupConfig.logLevel startupConfig
            , logOutput = StartupConfig.logOutput startupConfig
            , execHist = mempty
            , traceHist = mempty
            }

-- | Reset anything context-based in a 'InterpState'.
contextReset :: (Monoid a) => InterpState a -> InterpState a
contextReset state =
    state
        { func = Nothing
        , pauseLoc = Nothing
        , stack = mempty
        , bindings = Right mempty
        , status = Right mempty
        , traceHist = mempty
        }

-- | Append a string to the interpreter's history.
appendExecHist :: T.Text -> InterpState a -> InterpState a
appendExecHist cmd s@InterpState{execHist} = s{execHist = cmd : execHist}

{- | Is the daemon currently in the middle of an expression evaluation, but paused?
     Note, this does not indicate whether there's a scheduled 'DaemonIO' operation,
     but rather just indicates whether we have stopped at a breakpoint in the middle
     of evaluation.
-}
isExecuting :: InterpState a -> Bool
isExecuting InterpState{func = Nothing} = False
isExecuting InterpState{func = Just _} = True

-- | Start up the GHCi Daemon.
startup
    :: String
    -- ^ Command to run (e.g. "ghci" or "cabal repl")
    -> FilePath
    -- ^ Working directory to run the start up command in.
    -> StartupConfig
    -- ^ Where do we put the logging?
    -> DaemonIO (InterpState ())
    -- ^ The newly created interpreter handle.
startup cmd wd logOutput = do
    -- We don't want any highlighting or colours.
    let realCmd = "env TERM='dumb' " <> cmd
    state <- liftIO $ do
        (ghci, _) <- Ghcid.startGhci realCmd (Just wd) startupStreamCallback
        emptyInterpreterState ghci logOutput
    logDebug "|startup| GHCi Daemon initted" state
    updateState state

startupStreamCallback :: Ghcid.Stream -> String -> IO ()
startupStreamCallback stream msg = do
    IO.hPutStrLn handle [i|[ghcid startup:#{prefix}] #{msg}|]
    IO.hFlush handle
  where
    (handle, prefix) = case stream of
        Ghcid.Stdout -> (IO.stdout, "out" :: String)
        Ghcid.Stderr -> (IO.stderr, "err" :: String)

-- | Shut down the GHCi Daemon.
quit :: InterpState a -> IO (InterpState a)
quit state = do
    Ghcid.quit state._ghci
    pure state

-- | Update the interpreter state. Wrapper around other updaters.
updateState :: (Monoid a) => InterpState a -> DaemonIO (InterpState a)
updateState state =
    updateContext state
        >>= updateBindingsWithErrorHandling
        >>= updateModuleFileMap
        >>= updateBreakList
        >>= updateTraceHistory
  where
    -- Make a wrapper so we don't fail on updating bindings.
    -- Parsing bindings turns out to be actually impossible to solve
    -- with the current ':show bindings' output, so try our best
    -- and keep going.
    updateBindingsWithErrorHandling s = updateBindings s `catchE` catchBindings s
    catchBindings s er = pure s{bindings = Left er}

-- | Update the current interpreter context.
updateContext :: (Monoid a) => InterpState a -> DaemonIO (InterpState a)
updateContext state@InterpState{_ghci} = do
    logDebug "|updateContext| CMD: :show context\n" state
    msgs <- liftIO $ Ghcid.exec _ghci ":show context"
    let feedback = ParseContext.cleanResponse (T.pack <$> msgs)
    logDebug
        ( "|updateContext| OUT:\n"
            <> Util.linesToText msgs
            <> "\n"
        )
        state
    if T.null feedback
        then pure $ contextReset state -- We exited everything.
        else do
            let ctx = ParseContext.parseContext feedback
            case ctx of
                ParseContext.PCError er -> do
                    let msg = [i|Failed to update context: #{er}|]
                    logError ("|updateContext| " <> msg) state
                    throwE $ UpdateContextError msg
                ParseContext.PCNoContext -> pure $ contextReset state
                ParseContext.PCContext
                    ParseContext.ParseContextOut{func, filepath, pcSourceRange} ->
                        pure
                            state
                                { func = Just func
                                , pauseLoc = Just $ Loc.FileLoc filepath pcSourceRange
                                }

-- | Update the current local bindings.
updateBindings :: InterpState a -> DaemonIO (InterpState a)
updateBindings state@InterpState{_ghci} = do
    logDebug "|updateBindings| CMD: :show bindings\n" state
    msgs <- liftIO (Ghcid.exec _ghci ":show bindings")
    let feedback = ParseContext.cleanResponse (T.pack <$> msgs)
    logDebug
        ( "|updateBindings| OUT:\n"
            <> Util.linesToText msgs
            <> "\n"
        )
        state
    case ParseContext.parseBindings feedback of
        Right bindings -> pure (state{bindings = pure bindings})
        Left er -> do
            logError ("|updateBingings| " <> msg) state
            throwE $ UpdateBindingError msg
          where
            msg = [i|Failed to update bindings: #{er}|]

-- | Update the source map given any app state changes.
updateModuleFileMap :: InterpState a -> DaemonIO (InterpState a)
updateModuleFileMap state@InterpState{_ghci, moduleFileMap} = do
    logDebug "updateModuleFileMap|: CMD: :show modules\n" state
    msgs <- liftIO $ Ghcid.exec _ghci ":show modules"
    let packedMsgs = Util.linesToText msgs
    logDebug [i||updateModuleFileMap|: OUT: #{packedMsgs}\n|] state
    modules <- case ParseContext.parseShowModules packedMsgs of
        Right modules -> pure modules
        Left er -> throwE (GenericError (showT er))
    logDebug [i||updateModuleFileMap| modules: #{modules}|] state
    let addedModuleMap = Loc.moduleFileMapFromList modules
    let newModuleFileMap = addedModuleMap <> moduleFileMap
    pure $ state{moduleFileMap = newModuleFileMap}

updateTraceHistory :: InterpState a -> DaemonIO (InterpState a)
updateTraceHistory state = do
    (newState, eTraceHist) <- history state
    pure $ case eTraceHist of
        Left _ -> newState{traceHist = []}
        Right traceHist -> newState{traceHist}

-- | Analogue to @:step@.
step :: (Monoid a) => InterpState a -> ExceptT DaemonError IO (InterpState a)
step = execMuted ":step"

-- | Analogue to @:step <func>@.
stepInto
    :: (Monoid a)
    => T.Text
    -> InterpState a
    -- ^ Function name to jump to.
    -> ExceptT DaemonError IO (InterpState a)
    -- ^ New interpreter state.
stepInto func = execMuted (":step " <> func)

{- | Analogue to @:history@.
     Returns either a 'Left' error message, or a 'Right' list of trace breakpoints.
-}
history :: InterpState a -> DaemonIO (InterpState a, Either T.Text [T.Text])
history state = do
    msgStrs <- liftIO $ Ghcid.exec (_ghci state) ":history"
    let msgs = T.lines (ParseContext.cleanResponse (T.pack <$> msgStrs))
    logDebug [i||history| OUT:\n#{T.unlines msgs}|] state
    case msgs of
        [] -> throwE (GenericError "':history' unexpectedly returned nothing.")
        [oneLine] ->
            if ParseContext.isHistoryFailureMsg oneLine
                then -- This is probably an error message. Set it as such.
                    pure (state, Left oneLine)
                else -- This is a real trace entry... maybe.
                    pure (state, Right [oneLine])
        _ -> pure (state, Right msgs)

-- | Analogue to @:continue@. Throws out any messages.
continue :: (Monoid a) => InterpState a -> DaemonIO (InterpState a)
continue = execMuted ":continue"

-- | Analogue to @:trace@, with no arguments. Throws out any messages.
trace :: (Monoid a) => InterpState a -> DaemonIO (InterpState a)
trace = execMuted ":trace"

-- | Analogue to @:load <filepath>@. Throws out any messages.
load :: (Monoid a) => FilePath -> InterpState a -> DaemonIO (InterpState a)
load filepath = execMuted (T.pack $ ":load " <> filepath)

{- | Return tab completions for a given prefix.
     Analog to @:complete repl "\<prefix\>"@
     See https://downloads.haskell.org/ghc/latest/docs/users_guide/ghci.html#ghci-cmd-:complete
-}
tabComplete
    :: (Monoid a)
    => T.Text
    -- ^ Text (prefix) to return autocompletions of. Does not need to be escaped.
    -> InterpState a
    -- ^ Interpreter state to use.
    -> DaemonIO (InterpState a, (T.Text, [T.Text]))
    -- ^ Resulting state, the prefix, and autocompletions.
tabComplete providedPrefix state = do
    -- Tab completion expects input to be 'show'n in quotes.
    -- There's probably a better way of doing this!
    let escapedPrefix = Util.showT providedPrefix
    let cmd = ":complete repl " <> escapedPrefix
    (newState, outputLines) <- execCleaned cmd state
    (prefix, completions) <- case ParseTabCompletions.parseCompletionsWithHeader outputLines of
        Right c -> pure c
        Left (ParseTabCompletions.ParseError er) -> throwE (GenericError er)
    pure (newState, (prefix, completions))

-- -------------------------------------------------------------------------------------------------

{- | Execute an arbitrary command, as if it was directly written in GHCi.
     It is unlikely you want to call this directly, and instead want to call
     one of the wrapped functions or 'execMuted' or 'execCleaned'.
-}
exec :: (Monoid a) => T.Text -> InterpState a -> ExceptT DaemonError IO (InterpState a, [T.Text])
exec cmd state@InterpState{_ghci} = do
    logDebug ("|exec| CMD: " <> cmd) state
    msgs <- liftIO $ Ghcid.exec _ghci (T.unpack cmd)
    logDebug [i||exec| OUT:\n#{Util.linesToText msgs}\n|] state
    newState <-
        updateState
            ( -- Only append the command to the history if it has something interesting.
              if T.null cmd
                then state
                else appendExecHist cmd state
            )
    pure (newState, fmap T.pack msgs)

-- | 'exec', but throw out any messages.
execMuted :: (Monoid a) => T.Text -> InterpState a -> ExceptT DaemonError IO (InterpState a)
execMuted cmd state = fst <$> exec cmd state

-- | 'exec', but fully clean the message from prompt.
execCleaned
    :: (Monoid a)
    => T.Text
    -> InterpState a
    -> ExceptT DaemonError IO (InterpState a, [T.Text])
execCleaned cmd state = do
    res <- cleaner <$> exec cmd state
    logDebug ("|cleaned|:\n" <> (T.unlines . snd $ res)) state
    pure res
  where
    cleaner (s, ls) = (s, T.lines (ParseContext.cleanResponse ls))

-- ------------------------------------------------------------------------------------------------
-- Breakpoint handling
-- ------------------------------------------------------------------------------------------------

-- | Location info passed to breakpoint functions.
data BreakpointArg
    = -- | Location in the current file.
      LocalLine !Int
    | -- | Location in a module.
      ModLoc !Loc.ModuleLoc
    deriving (Show, Eq, Ord)

-- | Toggle a breakpoint (disable/enable) at a given location.
toggleBreakpointLine :: (Monoid a) => BreakpointArg -> InterpState a -> DaemonIO (InterpState a)
toggleBreakpointLine loc state
    | Right True <- isSet = deleteBreakpointLine loc state
    | Left x <- isSet = throwE x
    | otherwise = setBreakpointLine loc state
  where
    handleModLoc ml =
        fileLoc >>= \fl -> case (Loc.filepath fl, Loc.startLine (Loc.sourceRange fl)) of
            (filepath, Just lineno) ->
                Right $ lineno `elem` getBpInFile filepath state
            (_, _) -> invalidLoc ml
      where
        fileLoc = maybe (invalidLoc ml) Right (Loc.toFileLoc (moduleFileMap state) ml)

    isSet =
        case loc of
            LocalLine lineno -> Right $ lineno `elem` getBpInCurModule state
            ModLoc ml -> handleModLoc ml

    invalidLoc :: Loc.ModuleLoc -> Either DaemonError a
    invalidLoc ml =
        Left $
            BreakpointError [i|Cannot locate breakpoint position '#{ml}' in module without source|]

-- | Set a breakpoint at a given line.
setBreakpointLine :: (Monoid a) => BreakpointArg -> InterpState a -> DaemonIO (InterpState a)
setBreakpointLine loc state = do
    command <- getCommand
    execMuted command state
  where
    getCommand :: DaemonIO T.Text
    getCommand = do
        breakPos <- case loc of
            LocalLine pos -> pure (showT pos)
            ModLoc (Loc.ModuleLoc mod' Loc.SourceRange{startLine, startCol}) ->
                let line = maybe "" showT startLine
                    colno = maybe "" showT startCol
                 in if line == ""
                        then
                            throwE
                                (BreakpointError "Cannot set breakpoint at unknown line number")
                        else pure [i|#{mod'} #{line} #{colno}|]
        pure (":break " <> breakPos)

-- | Delete a breakpoint at a given line.
deleteBreakpointLine :: (Monoid a) => BreakpointArg -> InterpState a -> DaemonIO (InterpState a)
deleteBreakpointLine loc state =
    let convert (LocalLine ll) =
            -- TODO: We really should not consider LocalLines valid for this, because we don't
            -- really know whether it's local to the paused file, or local to the file
            -- we're viewing.
            -- But that's a problem for future me.
            let fakeSourceRange = Loc.srFromLineNo ll
             in do
                    pauseLoc <- state.pauseLoc
                    Loc.toModuleLoc
                        state.moduleFileMap
                        (pauseLoc{Loc.fSourceRange = fakeSourceRange})
        convert (ModLoc ml) = Just ml

        -- Get the breakpoint index if it exists.
        idxMaybe =
            convert loc >>= \ml ->
                let match x y =
                        let srX = Loc.sourceRange x
                            srY = Loc.sourceRange y
                         in Loc.startLine srX == Loc.startLine srY
                                && Loc.endLine srY == Loc.endLine srY
                 in headMay
                        [ idx
                        | (idx, otherML) <- state.breakpoints
                        , match ml otherML
                        ]
     in case idxMaybe of
            Just num -> execMuted (":delete " <> showT num) state
            Nothing -> do
                logDebug
                    ( [i|No breakpoint at '#{show loc}'; |]
                        <> [i|breakpoints are found at #{show (breakpoints state)}|]
                    )
                    state
                pure state

updateBreakList :: InterpState a -> ExceptT DaemonError IO (InterpState a)
updateBreakList state@InterpState{_ghci} = do
    logDebug "|updateBreakList| CMD: :show breaks\n" state
    msgs <- liftIO (Ghcid.exec _ghci ":show breaks")
    logDebug
        ( "|updateBreakList| OUT:\n"
            <> Util.linesToText msgs
        )
        state
    let response = ParseContext.cleanResponse (T.pack <$> msgs)
    case ParseContext.parseShowBreaks response of
        Right breakpoints -> pure state{breakpoints}
        Left er -> throwE (UpdateBreakListError [i|parsing breakpoint list: #{er}|])

-- | Return a list of breakpoint line numbers in the currently paused file.
getBpInCurModule :: InterpState a -> [Int]
getBpInCurModule InterpState{pauseLoc = Nothing} = []
getBpInCurModule s@InterpState{pauseLoc = Just Loc.FileLoc{filepath = fp}} = getBpInFile fp s

-- | Return a list of breakpoint line numbers in the given filepath.
getBpInFile :: FilePath -> InterpState a -> [Int]
getBpInFile fp state =
    catMaybes
        [ Loc.startLine (Loc.sourceRange loc)
        | loc <- breakpointlocs
        , Loc.filepath loc == fp
        ]
  where
    -- Convert between module locations and file locations
    convert (_, x) = Loc.toFileLoc (moduleFileMap state) x
    breakpointlocs = mapMaybe convert (breakpoints state)

-- ------------------------------------------------------------------------------------------------

-- | Log a message at the Debug level.
logDebug :: (MonadIO m) => T.Text -> InterpState a -> m ()
logDebug msg state =
    liftIO $ do
        when (logLevel state >= LogLevel 2) $
            logHelper output "[DEBUG]: " msg
  where
    output = logOutput state

-- Log a message at the Error level.
logError :: (MonadIO m) => T.Text -> InterpState a -> m ()
logError msg state =
    liftIO $ do
        when (logLevel state >= LogLevel 0) $
            logHelper output "[ERROR]: " msg
  where
    output = logOutput state

logHelper
    :: (MonadIO m)
    => LogOutput
    -- ^ Where to log?
    -> T.Text
    -- ^ prefix
    -> T.Text
    -- ^ Message
    -> m ()
logHelper outputLoc prefix msg = do
    liftIO $ case outputLoc of
        LogOutputFile path -> T.appendFile path fmtMsg
        LogOutputStdOut -> T.putStrLn fmtMsg
        LogOutputStdErr -> T.hPutStrLn IO.stderr fmtMsg
  where
    fmtMsg = T.unlines [prefix <> line | line <- T.lines msg]

-- ------------------------------------------------------------------------------------------------
-- Misc

data DaemonError
    = GenericError !T.Text
    | UpdateBindingError !T.Text
    | UpdateBreakListError !T.Text
    | BreakpointError !T.Text
    | UpdateContextError !T.Text
    deriving (Eq, Show)

{- | An IO operation that can fail into a DaemonError.
     Execute them synchronously in IO through 'run'.
     Execute them asynchronously in IO through 'schedule'.
-}
type DaemonIO r = ExceptT DaemonError IO r

-- | Convert Daemon operation to an IO operation.
run :: DaemonIO r -> IO (Either DaemonError r)
run = runExceptT

-- | Schedule execution of this Daemon IO operation, with the result being stored in the MVar.
schedule :: DaemonIO r -> IO (MVar (Either DaemonError r))
schedule daemonIO = do
    opResultVar <- newEmptyMVar
    _ <- forkIO $ ioOp opResultVar
    pure opResultVar
  where
    ioOp opResultVar = do
        result <- run daemonIO
        putMVar opResultVar result

-- | Schedule execution of this Daemon IO operation, and
scheduleWithCb :: InterpState a -> DaemonIO r -> (Either DaemonError r -> IO ()) -> IO ()
scheduleWithCb state daemonIO callback = do
    _ <- forkIO $ do
        _ <- takeMVar (_ghciLock state)
        result <- run daemonIO
        _ <- putMVar (_ghciLock state) ()
        callback result
    pure ()

-- | Stop the currently executing process in the daemon.
interruptDaemon :: InterpState a -> IO ()
interruptDaemon InterpState{_ghci} = Ghcid.interrupt _ghci
