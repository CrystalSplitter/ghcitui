{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}

module Ghcid.Daemon
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
        )
    , emptyInterpreterState

      -- * Startup and shutdown
    , startup
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

      -- * Misc
    , isExecuting
    , BreakpointArg (..)
    , run
    , DaemonIO
    , DaemonError
    ) where

import Control.Error
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.String.Interpolate (i)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Language.Haskell.Ghcid as Ghcid
import System.IO (stderr)

import qualified Ghcid.ParseContext as ParseContext
import qualified Loc
import qualified NameBinding
import Util (showT)
import qualified Util

newtype LogLevel = LogLevel Int deriving (Eq, Ord, Show)

-- | Determines where the daemon logs are written.
data LogOutput = LogOutputStdOut | LogOutputStdErr | LogOutputFile FilePath

data InterpState a = InterpState
    { _ghci :: Ghcid.Ghci
    -- ^ GHCiD handle.
    , func :: !(Maybe T.Text)
    -- ^ Current pause position function name.
    , pauseLoc :: !(Maybe Loc.FileLoc)
    -- ^ Current pause position.
    , moduleFileMap :: !Loc.ModuleFileMap
    -- ^ Mapping between modules and their filepaths.
    , stack :: [T.Text]
    -- ^ Program stack (only available during tracing).
    , breakpoints :: [(Int, Loc.ModuleLoc)]
    -- ^ Currently set breakpoint locations.
    , bindings :: Either DaemonError [NameBinding.NameBinding T.Text]
    -- ^ Current context value bindings.
    , status :: !(Either T.Text a)
    -- ^ IDK? I had an idea here at one point.
    , logLevel :: !LogLevel
    -- ^ How much should we log?
    , logOutput :: !LogOutput
    -- ^ Where should we log to?
    , execHist :: [T.Text]
    -- ^ What's the execution history?
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
                _ -> "<unknown pause location>" :: String
         in msg

{- | Create an empty/starting interpreter state.
Usually you don't want to call this directly. Instead use 'startup'.
-}
emptyInterpreterState :: (Monoid a) => Ghcid.Ghci -> InterpState a
emptyInterpreterState ghci =
    InterpState
        { _ghci = ghci
        , func = Nothing
        , pauseLoc = Nothing
        , moduleFileMap = mempty
        , stack = mempty
        , breakpoints = mempty
        , bindings = Right mempty
        , status = Right mempty
        , logLevel = LogLevel 3
        , logOutput = LogOutputFile "/tmp/ghcitui.log"
        , execHist = mempty
        }

-- | Append a string to the interpreter's history.
appendExecHist :: T.Text -> InterpState a -> InterpState a
appendExecHist cmd s@InterpState{execHist} = s{execHist = cmd : execHist}

-- | Is the daemon currently in the middle of an expression evaluation?
isExecuting :: InterpState a -> Bool
isExecuting InterpState{func = Nothing} = False
isExecuting InterpState{func = Just _} = True

-- | Start up the GHCi Daemon.
startup
    :: String
    -- ^ Command to run (e.g. "ghci" or "cabal repl")
    -> FilePath
    -- ^ Working directory to run the start up command in.
    -> DaemonIO (InterpState ())
    -- ^ The newly created interpreter handle.
startup cmd pwd = do
    let startOp = Ghcid.startGhci cmd (Just pwd) (\_ _ -> pure ())
    (ghci, _) <- liftIO startOp
    updateState (emptyInterpreterState ghci)

-- | Shut down the GHCi Daemon.
quit :: InterpState a -> IO (InterpState a)
quit state = do
    Ghcid.quit (state._ghci)
    pure state

-- | Update the interpreter state. Wrapper around other updaters.
updateState :: (Monoid a) => InterpState a -> DaemonIO (InterpState a)
updateState state =
    updateContext state
        >>= updateBindingsWithErrorHandling
        >>= updateModuleFileMap
        >>= updateBreakList
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
    logDebug state "|updateContext| CMD: :show context\n"
    msgs <- liftIO $ Ghcid.exec _ghci ":show context"
    let feedback = ParseContext.cleanResponse (T.pack <$> msgs)
    logDebug
        state
        ( "|updateContext| OUT:\n"
            <> StringUtil.linesToText msgs
            <> "\n"
        )
    if T.null feedback
        then pure (emptyInterpreterState _ghci) -- We exited everything.
        else do
            let ctx = ParseContext.parseContext feedback
            case ctx of
                ParseContext.PCError er -> error [i|Failed to update context: #{er}|]
                ParseContext.PCNoContext -> pure (emptyInterpreterState _ghci)
                ParseContext.PCContext ParseContext.ParseContextOut{func, filepath, pcSourceRange} ->
                    pure state{func = Just func, pauseLoc = Just $ Loc.FileLoc filepath pcSourceRange}

-- | Update the current local bindings.
updateBindings :: InterpState a -> DaemonIO (InterpState a)
updateBindings state@InterpState{_ghci} = do
    logDebug state "|updateBindings| CMD: :show bindings\n"
    msgs <- liftIO (Ghcid.exec _ghci ":show bindings")
    let feedback = ParseContext.cleanResponse (T.pack <$> msgs)
    logDebug
        state
        ( "|updateBindings| OUT:\n"
            <> StringUtil.linesToText msgs
            <> "\n"
        )
    case ParseContext.parseBindings feedback of
        Right bindings -> pure (state{bindings = pure bindings})
        Left er -> throwE (UpdateBindingError [i|Failed to update bindings: #{er}|])

-- | Update the source map given any app state changes.
updateModuleFileMap :: InterpState a -> DaemonIO (InterpState a)
updateModuleFileMap state@InterpState{_ghci, moduleFileMap} = do
    logDebug state "updateModuleFileMap|: CMD: :show modules\n"
    msgs <- liftIO $ Ghcid.exec _ghci ":show modules"
    let packedMsgs = StringUtil.linesToText msgs
    logDebug state [i||updateModuleFileMap|: OUT: #{packedMsgs}\n|]
    modules <- case ParseContext.parseShowModules packedMsgs of
        Right modules -> pure modules
        Left er -> error $ show er
    logDebug state [i||updateModuleFileMap| modules: #{modules}|]
    let addedModuleMap = Loc.moduleFileMapFromList modules
    let newModuleFileMap = addedModuleMap <> moduleFileMap
    pure $ state{moduleFileMap = newModuleFileMap}

-- | Analogue to @:step@.
step :: (Monoid a) => InterpState a -> ExceptT DaemonError IO (InterpState a)
step state = execMuted state ":step"

-- | Analogue to @:step <func>@.
stepInto
    :: (Monoid a)
    => InterpState a
    -> T.Text
    -- ^ Function name to jump to.
    -> ExceptT DaemonError IO (InterpState a)
    -- ^ New interpreter state.
stepInto state func = execMuted state (":step " <> func)

-- | Analogue to @:continue@. Throws out any messages.
continue :: (Monoid a) => InterpState a -> DaemonIO (InterpState a)
continue state = execMuted state ":continue"

-- | Analogue to @:load <filepath>@. Throws out any messages.
load :: (Monoid a) => InterpState a -> FilePath -> DaemonIO (InterpState a)
load state filepath = execMuted state (T.pack $ ":load " <> filepath)

{- | Execute an arbitrary command, as if it was directly written in GHCi.
It is unlikely you want to call this directly, and instead want to call
one of the wrapped functions or 'execMuted' or 'execCleaned'.
-}
exec :: (Monoid a) => InterpState a -> T.Text -> ExceptT DaemonError IO (InterpState a, [T.Text])
exec state@InterpState{_ghci} cmd = do
    logDebug state ("|exec| CMD: " <> cmd)
    msgs <- liftIO $ Ghcid.exec _ghci (T.unpack cmd)
    logDebug state [i|{|exec| OUT:\n#{StringUtil.linesToText msgs}\n}|]
    newState <- updateState $ appendExecHist cmd state
    pure (newState, fmap T.pack msgs)

-- | 'exec', but throw out any messages.
execMuted :: (Monoid a) => InterpState a -> T.Text -> ExceptT DaemonError IO (InterpState a)
execMuted state cmd = do
    (newState, _) <- exec state cmd
    pure newState

-- | 'exec', but fully clean the message from prompt.
execCleaned
    :: (Monoid a)
    => InterpState a
    -> T.Text
    -> ExceptT DaemonError IO (InterpState a, [T.Text])
execCleaned state cmd = do
    res <- cleaner <$> exec state cmd
    logDebug state ("|cleaned|:\n" <> (T.unlines . snd $ res))
    pure res
  where
    cleaner (s, ls) = (s, T.lines (ParseContext.cleanResponse ls))

-- | Location info passed to breakpoint functions.
data BreakpointArg
    = -- | Location in the current file.
      LocalLine !Int
    | -- | Location in a module.
      ModLoc Loc.ModuleLoc
    deriving (Show, Eq, Ord)

-- | Toggle a breakpoint (disable/enable) at a given location.
toggleBreakpointLine :: (Monoid a) => InterpState a -> BreakpointArg -> DaemonIO (InterpState a)
toggleBreakpointLine state loc
    | Right True <- isSet = deleteBreakpointLine state loc
    | Left x <- isSet = throwE x
    | otherwise = setBreakpointLine state loc
  where
    invalidLoc :: Loc.ModuleLoc -> Either DaemonError a
    invalidLoc ml = Left $ BreakpointError [i|Cannot locate breakpoint position '#{ml}' in module without source|]

    handleModLoc ml =
        fileLoc >>= \fl -> case (Loc.filepath fl, Loc.startLine (Loc.sourceRange fl)) of
            (filepath, Just lineno) ->
                Right $ lineno `elem` getBpInFile state filepath
            (_, _) -> invalidLoc ml
      where
        fileLoc = maybe (invalidLoc ml) Right (Loc.toFileLoc state.moduleFileMap ml)

    isSet =
        case loc of
            LocalLine lineno -> Right $ lineno `elem` getBpInCurModule state
            ModLoc ml -> handleModLoc ml

-- | Set a breakpoint at a given line.
setBreakpointLine :: (Monoid a) => InterpState a -> BreakpointArg -> DaemonIO (InterpState a)
setBreakpointLine state loc = do
    command <- getCommand
    execMuted state command
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
                        else pure (mod' <> " " <> line <> " " <> colno)
        pure (":break " <> breakPos)

-- | Delete a breakpoint at a given line.
deleteBreakpointLine :: (Monoid a) => InterpState a -> BreakpointArg -> DaemonIO (InterpState a)
deleteBreakpointLine state loc =
    let convert (LocalLine ll) =
            -- TODO: We really should not consider LocalLines valid for this, because we don't
            -- really know whether it's local to the paused file, or local to the file
            -- we're viewing.
            -- But that's a problem for future me.
            let fakeSourceRange =
                    Loc.SourceRange
                        { startLine = Just ll
                        , endLine = Just ll
                        , startCol = Nothing
                        , endCol = Nothing
                        }
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
            Just num -> execMuted state (":delete " <> showT num)
            Nothing -> do
                logDebug
                    state
                    ( [i|No breakpoint at '#{show loc}'; |]
                        <> [i|breakpoints are found at #{show (breakpoints state)}|]
                    )
                pure state

updateBreakList :: InterpState a -> ExceptT DaemonError IO (InterpState a)
updateBreakList state@InterpState{_ghci} = do
    logDebug state "|updateBreakList| CMD: :show breaks\n"
    msgs <- liftIO (Ghcid.exec _ghci ":show breaks")
    logDebug
        state
        ( "|updateBreakList| OUT:\n"
            <> StringUtil.linesToText msgs
        )
    let response = ParseContext.cleanResponse (T.pack <$> msgs)
    case ParseContext.parseShowBreaks response of
        Right breakpoints -> pure state{breakpoints}
        Left er -> throwE (UpdateBreakListError [i|parsing breakpoint list: #{er}|])

-- | Return a list of breakpoint line numbers in the currently paused file.
getBpInCurModule :: InterpState a -> [Int]
getBpInCurModule InterpState{pauseLoc = Nothing} = []
getBpInCurModule s@InterpState{pauseLoc = Just Loc.FileLoc{filepath = fp}} = getBpInFile s fp

-- | Return a list of breakpoint line numbers in the given filepath.
getBpInFile :: InterpState a -> FilePath -> [Int]
getBpInFile s fp =
    catMaybes
        [ Loc.startLine (Loc.sourceRange loc)
        | loc <- breakpointlocs
        , Loc.filepath loc == fp
        ]
  where
    -- Convert between module locations and file locations
    convert (_, x) = Loc.toFileLoc s.moduleFileMap x
    breakpointlocs = mapMaybe convert s.breakpoints

-- ------------------------------------------------------------------------------------------------

-- | Log a message at the Debug level.
logDebug :: (MonadIO m) => InterpState a -> T.Text -> m ()
logDebug s msg =
    liftIO $ do
        when (num >= 3) $
            logHelper output "[DEBUG]: " msg
  where
    LogLevel num = s.logLevel
    output = logOutput s

-- | Log a message at the Error level.
logError :: (MonadIO m) => InterpState a -> T.Text -> m ()
logError s msg =
    liftIO $ do
        when (num >= 0) $
            logHelper output "[ERROR]: " msg
  where
    LogLevel num = s.logLevel
    output = logOutput s

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
        LogOutputStdErr -> T.hPutStrLn stderr fmtMsg
  where
    fmtMsg = T.unlines [prefix <> line | line <- T.lines msg]

-- ------------------------------------------------------------------------------------------------
-- Misc

showT :: (Show a) => a -> T.Text
showT = T.pack . show

data DaemonError
    = GenericError T.Text
    | UpdateBindingError T.Text
    | UpdateBreakListError T.Text
    | BreakpointError T.Text
    deriving (Show, Eq)

type DaemonIO r = ExceptT DaemonError IO r

-- | Convert Daemon operation to an IO operation.
run :: DaemonIO r -> IO (Either DaemonError r)
run = runExceptT
