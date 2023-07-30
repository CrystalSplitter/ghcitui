module AppTopLevel (AppName(..), Command) where

data AppName = GHCiTUI | CodeViewport | LiveInterpreter deriving (Eq, Show, Ord)

type Command = String
