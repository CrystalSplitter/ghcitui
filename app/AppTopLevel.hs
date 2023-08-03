module AppTopLevel (AppName(..), Command) where

data AppName = GHCiTUI | CodeViewport | LiveInterpreter | LiveInterpreterViewport deriving (Eq, Show, Ord)

type Command = String
