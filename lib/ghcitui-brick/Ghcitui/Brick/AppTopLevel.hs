module Ghcitui.Brick.AppTopLevel (AppName (..)) where

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
