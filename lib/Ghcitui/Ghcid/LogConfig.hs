module Ghcitui.Ghcid.LogConfig where

-- | Determines how verbose logging should be.
newtype LogLevel = LogLevel Int deriving (Eq, Ord, Show)

-- | Determines where the daemon logs are written.
data LogOutput = LogOutputStdOut | LogOutputStdErr | LogOutputFile FilePath deriving (Show)