module Ghcitui.Brick.AppTopLevel (AppName (..), CustomAppEvent (..)) where

import qualified Data.Text as T

import qualified Ghcitui.Loc as Loc

-- | Unique identifiers for components of the App.
data AppName
    = GHCiTUI
    | CodeViewport
    | SourceWindowLine Int
    | DebugPanel
    | LiveInterpreter
    | LiveInterpreterViewport
    | HelpViewport
    | BindingViewport
    | ModulesViewport
    | TraceViewport
    | -- | Source Window Name.
      SourceList
    deriving (Eq, Show, Ord)

-- Aliases so it's easier to identify the names of

type AppEventCmd = T.Text
type AppEventLogs = [T.Text]
type AppEventPrefix = T.Text
type AppEventCompletions = [T.Text]

-- | Callback event types.
data CustomAppEvent state
    = ErrorOnCb state T.Text
    | StepCb state
    | BreakpointCb state Loc.ModuleLoc
    | ReplExecCb state AppEventCmd AppEventLogs
    | ReplTabCompleteCb state AppEventCmd (AppEventPrefix, AppEventCompletions)
    deriving (Eq, Show, Ord)
