module AppTopLevel (AppName (..)) where

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
    | SourceList
    deriving (Eq, Show, Ord)