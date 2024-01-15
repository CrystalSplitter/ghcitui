module Ghcitui.Ghcid.StartupConfig (StartupConfig (..)) where

import Ghcitui.Ghcid.LogConfig (LogLevel, LogOutput)

-- | Configuration passed during Daemon 'startup'
data StartupConfig = StartupConfig
    { logLevel :: !LogLevel
    -- ^ How much do we want to log?
    , logOutput :: !LogOutput
    -- ^ Where do we log?
    }
