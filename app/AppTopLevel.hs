module AppTopLevel (AppName (..)) where

-- | Unique identifiers for components of the App.
data AppName
    = GHCiTUI
    | CodeViewport
    | CodeViewportLine Int
    | LiveInterpreter
    | LiveInterpreterViewport
    deriving (Eq, Show, Ord)
