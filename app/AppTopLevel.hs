module AppTopLevel (AppName (..)) where

-- | Unique identifiers for components of the App.
data AppName
    = GHCiTUI
    | CodeViewport
    | LiveInterpreter
    | LiveInterpreterViewport
    deriving (Eq, Show, Ord)
