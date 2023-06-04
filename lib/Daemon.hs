{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}

module Daemon
    ( CodeLine (..)
    , startup
    , exec
    , step
    , stepInto
    , setBreakpointLine
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

data InterpState a = InterpState
    { _ghci :: Ghcid.Ghci
    , func :: Maybe Text.Text
    , filepath :: Maybe FilePath
    , lineno :: Maybe Int
    , colrange :: (Maybe Int, Maybe Int)
    , stack :: [String]
    , breakpoints :: [CodeLine]
    , status :: Either Text.Text a
    }

instance Show (InterpState a) where
    show :: InterpState a -> String
    show s =
        let func' = show s.func
            filepath' = show s.filepath
            lineno' = show s.lineno
            colrange' = show s.colrange
         in [i|{func="#{func'}", filepath="#{filepath'}", lineno="#{lineno'}, colrange="#{colrange'}"}|]

emptyInterpreterState :: (Monoid a) => Ghcid.Ghci -> InterpState a
emptyInterpreterState ghci =
    InterpState
        { _ghci = ghci
        , func = Nothing
        , filepath = Nothing
        , lineno = Nothing
        , colrange = (Nothing, Nothing)
        , stack = []
        , breakpoints = []
        , status = Right mempty
        }

startup :: String -> FilePath -> IO (InterpState ())
startup cmd pwd = do
    (ghci, loadingMsgs) <- Ghcid.startGhci cmd (Just pwd) (\_ _ -> pure ())
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
                    , filepath = out.filepath
                    , lineno = out.lineno
                    , colrange = out.colrange
                    }

step :: (Monoid a) => InterpState a -> IO (InterpState a)
step state = execMuted state ":step"

stepInto :: (Monoid a) => InterpState a -> String -> IO (InterpState a)
stepInto state func = execMuted state (":step " ++ func)

continue :: (Monoid a) => InterpState a -> IO (InterpState a)
continue state = execMuted state ":continue"

load :: (Monoid a) => InterpState a -> FilePath -> IO (InterpState a)
load state filepath = execMuted state (":l " ++ filepath)

exec :: (Monoid a) => InterpState a -> String -> IO (InterpState a, [String])
exec state@InterpState{_ghci} cmd = do
    msgs <- Ghcid.exec _ghci cmd
    newState <- updateState state
    pure (newState, msgs)

execMuted :: (Monoid a) => InterpState a -> String -> IO (InterpState a)
execMuted state cmd = do
    (newState, _) <- exec state cmd
    pure newState

data CodeLine
    = LocalLine Int
    | ModuleLine
        { mod' :: Maybe String
        , lineno :: Int
        , colno :: Maybe Int
        }

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
