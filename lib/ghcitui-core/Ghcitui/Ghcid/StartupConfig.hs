module Ghcitui.Ghcid.StartupConfig (StartupConfig (..)) where

import qualified Data.Text as T
import Ghcitui.Ghcid.LogConfig (LogLevel, LogOutput)

-- | Configuration passed during Daemon 'startup'
data StartupConfig = StartupConfig
    { logLevel :: !LogLevel
    -- ^ How much do we want to log?
    , logOutput :: !LogOutput
    -- ^ Where do we log?
    , osType :: !(Maybe T.Text)
    {- ^ What operating system are we on? 'mingw32', 'linux', 'darwin' are known,
    should map to 'System.Info.os'.
    -}
    }
