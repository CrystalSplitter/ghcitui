{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}

module Daemon
    ( ModuleLoc (..)
    , startup
    , exec
    , step
    , stepInto
    , setBreakpointLine
    , getBpInCurFile
    , getBpInFile
    , quit
    , InterpState (..)
    , emptyInterpreterState
    , continue
    , load
    ) where

import Data.String.Interpolate (i)
import qualified Data.Text as Text
import qualified Language.Haskell.Ghcid as Ghcid

import qualified ParseContext as PC (ParseContextOut (..), linesToText, parseContext)
import Data.Maybe (catMaybes)

data InterpState a = InterpState
    { _ghci :: Ghcid.Ghci
    -- ^ GHCID handle
    , func :: Maybe Text.Text
    -- ^ Current pause position function name.
    , pauseLoc :: ModuleLoc
    -- ^ Current pause position.
    , stack :: [String]
    -- ^ Program stack (only available during tracing)
    , breakpoints :: [ModuleLoc]
    -- ^ Currently set breakpoint locations.
    , status :: Either Text.Text a
    -- ^ IDK? I had an idea here at one point.
    }

instance Show (InterpState a) where
    show s =
        let func' = show s.func
            ModuleLoc filepath' lineno' colrange' = s.pauseLoc
         in [i|{func="#{func'}", filepath="#{filepath'}", lineno="#{lineno'}", colrange="#{colrange'}"}|]

emptyInterpreterState :: (Monoid a) => Ghcid.Ghci -> InterpState a
emptyInterpreterState ghci =
    InterpState
        { _ghci = ghci
        , func = Nothing
        , pauseLoc = ModuleLoc Nothing Nothing (Nothing, Nothing)
        , stack = []
        , breakpoints = []
        , status = Right mempty
        }

startup :: String -> FilePath -> IO (InterpState ())
startup cmd pwd = do
    (ghci, _) <- Ghcid.startGhci cmd (Just pwd) (\_ _ -> pure ())
    pure $ emptyInterpreterState ghci

quit :: InterpState a -> IO (InterpState a)
quit state = do
    Ghcid.quit (state._ghci)
    pure state

updateState :: (Monoid a) => InterpState a -> IO (InterpState a)
updateState state@InterpState{_ghci} = do
    contextLines <- Ghcid.exec _ghci ":show context"
    if null contextLines
        then do
            pure (emptyInterpreterState _ghci) -- We exited everything.
        else do
            let out = PC.parseContext (PC.linesToText contextLines)
            pure
                state
                    { func = out.func
                    , pauseLoc = ModuleLoc out.filepath out.lineno out.colrange
                    }

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

-- | Analogue to ":continue".
continue :: (Monoid a) => InterpState a -> IO (InterpState a)
continue state = execMuted state ":continue"

-- | Analogue to ":load <filepath>"
load :: (Monoid a) => InterpState a -> FilePath -> IO (InterpState a)
load state filepath = execMuted state (":l " ++ filepath)

-- | Execute an arbitrary command, as if it was directly written in GHCi.
exec :: (Monoid a) => InterpState a -> String -> IO (InterpState a, [String])
exec state@InterpState{_ghci} cmd = do
    msgs <- Ghcid.exec _ghci cmd
    newState <- updateState state
    pure (newState, msgs)

-- | @exec@, but throw out any messages.
execMuted :: (Monoid a) => InterpState a -> String -> IO (InterpState a)
execMuted state cmd = do
    (newState, _) <- exec state cmd
    pure newState

type ColumnRange = (Maybe Int, Maybe Int)
data ModuleLoc = ModuleLoc
    { filepath :: Maybe FilePath
    , lineno :: Maybe Int
    , colrange :: ColumnRange
    }

data CodeLine
    = LocalLine !Int
    | ModuleLine (Maybe String) !Int (Maybe Int)

-- | Set a breakpoint at a given line.
setBreakpointLine :: (Monoid a) => InterpState a -> CodeLine -> IO (InterpState a)
setBreakpointLine state loc = do
    (newState, _) <- exec state command
    pure newState
  where
    command =
        ":break " ++ case loc of
            LocalLine pos -> show pos
            ModuleLine (Just mod') pos (Just colno) ->
                show mod' ++ show pos ++ show colno
            ModuleLine (Just mod') pos Nothing ->
                show mod' ++ show pos
            ModuleLine Nothing pos Nothing ->
                show pos
            ModuleLine Nothing pos (Just colno) ->
                show pos ++ show colno

getBpInCurFile :: InterpState a -> [Int]
getBpInCurFile InterpState{pauseLoc = ModuleLoc{filepath = Nothing }} = []
getBpInCurFile s@InterpState{pauseLoc = ModuleLoc{filepath = Just fp }} = getBpInFile s fp

getBpInFile :: InterpState a -> FilePath -> [Int]
getBpInFile s fp = catMaybes [loc.lineno | loc<-s.breakpoints, loc.filepath == Just fp]