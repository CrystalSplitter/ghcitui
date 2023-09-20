{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}

module Ghcid.Daemon
    ( startup
    , BreakpointArg (..)
    , InterpState (..)
    , continue
    , deleteBreakpointLine
    , emptyInterpreterState
    , exec
    , execCleaned
    , execMuted
    , getBpInCurModule
    , getBpInFile
    , isExecuting
    , load
    , quit
    , setBreakpointLine
    , step
    , stepInto
    , toggleBreakpointLine
    ) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (..))
import qualified Data.Bifunctor as Bifunctor
import Data.Maybe (catMaybes, mapMaybe)
import Data.String.Interpolate (i)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Language.Haskell.Ghcid as Ghcid
import Safe

import qualified Ghcid.ParseContext as ParseContext
import qualified Loc
import qualified NameBinding
import qualified StringUtil

newtype LogLevel = LogLevel Int

-- | Determines where the daemon logs are written.
data LogOutput = LogOutputStdOut | LogOutputStdErr | LogOutputFile FilePath

data InterpState a = InterpState
    { _ghci :: Ghcid.Ghci
    -- ^ GHCiD handle.
    , func :: Maybe T.Text
    -- ^ Current pause position function name.
    , pauseLoc :: Maybe Loc.FileLoc
    -- ^ Current pause position.
    , moduleFileMap :: Loc.ModuleFileMap
    -- ^ Mapping between modules and their filepaths.
    , stack :: [T.Text]
    -- ^ Program stack (only available during tracing).
    , breakpoints :: [(Int, Loc.ModuleLoc)]
    -- ^ Currently set breakpoint locations.
    , bindings :: [NameBinding.NameBinding T.Text]
    -- ^ Current context value bindings.
    , status :: Either T.Text a
    -- ^ IDK? I had an idea here at one point.
    , logLevel :: LogLevel
    -- ^ How much should we log?
    , logOutput :: LogOutput
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
                     in [i|{func="#{func'}", filepath="#{filepath'}", #{srcRngFmt}}|]
                _ -> "<unknown pause location>" :: String
         in msg

-- | Create an empty/starting interpreter state.
emptyInterpreterState :: (Monoid a) => Ghcid.Ghci -> InterpState a
emptyInterpreterState ghci =
    InterpState
        { _ghci = ghci
        , func = Nothing
        , pauseLoc = Nothing
        , moduleFileMap = mempty
        , stack = mempty
        , breakpoints = mempty
        , bindings = mempty
        , status = Right mempty
        , logLevel = LogLevel 3
        , logOutput = LogOutputFile "/tmp/ghcitui.log"
        , execHist = mempty
        }

-- | Append a string to the interpreter's history.
appendExecHist :: T.Text -> InterpState a -> InterpState a
appendExecHist cmd s@InterpState{execHist} = s{execHist = cmd : execHist}

isExecuting :: InterpState a -> Bool
isExecuting InterpState{func = Nothing} = False
isExecuting InterpState{func = Just _} = True

-- | Start up the GHCi Daemon.
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

-- | Shutdown GHCiD.
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
            let unwrapLog f wrapper = case f ctx of
                    Left (ParseContext.ParseError msg) -> do
                        logError state msg
                        pure Nothing
                    Right x -> pure (wrapper x)
            mFunc <- unwrapLog ParseContext.func Just
            filepath <- case ParseContext.filepath ctx of
                Left (ParseContext.ParseError msg) -> do
                    logError state msg
                    error ("parsing filepath: " <> T.unpack msg)
                Right x -> pure x
            sourceRange <- case ParseContext.pcSourceRange ctx of
                Left (ParseContext.ParseError msg) -> do
                    logError state msg
                    pure Loc.unknownSourceRange
                Right x -> pure x
            pure
                state
                    { func = mFunc
                    , pauseLoc = Just $ Loc.FileLoc filepath sourceRange
                    }

-- | Update the current local bindings.
updateBindings :: InterpState a -> IO (InterpState a)
updateBindings state@InterpState{_ghci} = do
    logDebug state "|updateBindings| CMD: :show bindings\n"
    msgs <- Ghcid.exec _ghci ":show bindings"
    let feedback = ParseContext.cleanResponse (T.pack <$> msgs)
    logDebug
        state
        ( "|updateBindings| OUT:\n"
            <> StringUtil.linesToText msgs
            <> "\n"
        )
    case ParseContext.parseBindings feedback of
        Right bindings -> pure (state{bindings})
        Left err -> error ("Failed to update bindings:\n" <> T.unpack err)

-- | Update the source map given any app state changes.
updateModuleFileMap :: InterpState a -> IO (InterpState a)
updateModuleFileMap state@InterpState{_ghci, moduleFileMap} = do
    modules <- Ghcid.showModules _ghci
    logDebug state ("|updateModuleFileMap| modules: " <> showT modules)
    let addedModuleMap = Loc.moduleFileMapFromList (Bifunctor.first T.pack <$> modules)
    let newModuleFileMap = addedModuleMap <> moduleFileMap
    pure $ state{moduleFileMap = newModuleFileMap}

-- | Analogue to ":step".
step :: (Monoid a) => InterpState a -> IO (InterpState a)
step state = execMuted state ":step"

-- | Analogue to ":step \<func\>".
stepInto
    :: (Monoid a)
    => InterpState a
    -> T.Text
    -- ^ Function name to jump to
    -> IO (InterpState a)
    -- ^ New interpreter state
stepInto state func = execMuted state (":step " <> func)

-- | Analogue to ":continue". Throws out any messages.
continue :: (Monoid a) => InterpState a -> IO (InterpState a)
continue state = execMuted state ":continue"

-- | Analogue to ":load \<filepath\>". Throws out any messages.
load :: (Monoid a) => InterpState a -> FilePath -> IO (InterpState a)
load state filepath = execMuted state (T.pack $ ":load " <> filepath)

-- | Execute an arbitrary command, as if it was directly written in GHCi.
exec :: (Monoid a) => InterpState a -> T.Text -> IO (InterpState a, [T.Text])
exec state@InterpState{_ghci} cmd = do
    logDebug state ("|exec| CMD: " <> cmd)
    msgs <- Ghcid.exec _ghci (T.unpack cmd)
    logDebug state [i|{|exec| OUT:\n#{StringUtil.linesToText msgs}\n}|]
    newState <- updateState $ appendExecHist cmd state
    pure (newState, fmap T.pack msgs)

-- | 'exec', but throw out any messages.
execMuted :: (Monoid a) => InterpState a -> T.Text -> IO (InterpState a)
execMuted state cmd = do
    (newState, _) <- exec state cmd
    pure newState

-- | 'exec', but clean the message from prompt.
execCleaned :: (Monoid a) => InterpState a -> T.Text -> IO (InterpState a, [T.Text])
execCleaned state cmd = do
    res <- cleaner <$> exec state cmd
    logDebug state ("|cleaned|:\n" <> (T.unlines . snd $ res))
    pure res
  where
    cleaner (s, ls) = (s, T.lines (ParseContext.cleanResponse ls))

-- | Location info passed to *BreakpointLine functions.
data BreakpointArg
    = -- | Location in the current file.
      LocalLine !Int
    | -- | Location in a module.
      ModLoc Loc.ModuleLoc
    deriving (Show, Eq, Ord)

-- | Toggle a breakpoint (disable/enable) at a given location.
toggleBreakpointLine :: (Monoid a) => InterpState a -> BreakpointArg -> IO (InterpState a)
toggleBreakpointLine state loc
    | Right True <- isSet = deleteBreakpointLine state loc
    | Left x <- isSet = error x
    | otherwise = setBreakpointLine state loc
  where
    invalidLoc :: Loc.ModuleLoc -> Either String a
    invalidLoc ml = Left [i|Cannot locate breakpoint position '#{ml}' in module without source|]

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

showT :: (Show a) => a -> T.Text
showT = T.pack . show

-- | Set a breakpoint at a given line.
setBreakpointLine :: (Monoid a) => InterpState a -> BreakpointArg -> IO (InterpState a)
setBreakpointLine state loc = execMuted state command
  where
    command =
        ":break " <> case loc of
            LocalLine pos -> showT pos
            ModLoc (Loc.ModuleLoc mod' Loc.SourceRange{startLine, startCol}) ->
                let line = maybe "" showT startLine
                    colno = maybe "" showT startCol
                 in if line == ""
                        then error "Cannot set breakpoint at unknown line number"
                        else mod' <> " " <> line <> " " <> colno

-- | Delete a breakpoint at a given line.
deleteBreakpointLine :: (Monoid a) => InterpState a -> BreakpointArg -> IO (InterpState a)
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

updateBreakList :: InterpState a -> IO (InterpState a)
updateBreakList state@InterpState{_ghci} = do
    logDebug state "|updateBreakList| CMD: :show breaks\n"
    msgs <- Ghcid.exec _ghci ":show breaks"
    logDebug
        state
        ( "|updateBreakList| OUT:\n"
            <> StringUtil.linesToText msgs
        )
    let response = ParseContext.cleanResponse (T.pack <$> msgs)
    pure
        ( case ParseContext.parseShowBreaks response of
            Right breakpoints -> state{breakpoints}
            Left err -> error ("parsing breakpoint list: " <> T.unpack err)
        )

-- | Return a list of breakpoint line numbers in the current file.
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
        _ -> error "Cannot log to that output configuration."
  where
    fmtMsg = T.unlines [prefix <> line | line <- T.lines msg]
