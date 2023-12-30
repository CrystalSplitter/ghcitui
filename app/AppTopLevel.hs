module AppTopLevel (AppName (..)) where

-- | Unique identifiers for components of the App.
data AppName
    = GHCiTUI
    | CodeViewport
    | CodeViewportLine Int
    | LiveInterpreter
    | LiveInterpreterViewport
    | BindingViewport
    | ModulesViewport
    | TraceViewport
    deriving (Eq, Show, Ord)
