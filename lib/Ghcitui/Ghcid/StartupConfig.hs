module Ghcitui.Ghcid.StartupConfig (StartupConfig(..)) where

import Ghcitui.Ghcid.LogConfig (LogLevel, LogOutput)

-- | Configuration passed during Daemon 'startup'
data StartupConfig = StartupConfig
    { logLevel :: !LogLevel
    , logOutput :: !LogOutput
    }