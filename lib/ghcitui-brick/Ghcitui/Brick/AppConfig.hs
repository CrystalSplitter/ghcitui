{-# LANGUAGE OverloadedStrings #-}

module Ghcitui.Brick.AppConfig
    ( AppConfig (..)
    , defaultConfig
    , loadStartupSplash
    , userConfigDir
    )
where

import Data.Maybe (fromMaybe)
import Data.String (IsString)
import qualified Data.Text as T
import System.Environment (lookupEnv)

import qualified Ghcitui.Brick.SplashTextEmbed as SplashTextEmbed

userConfigDir :: IO FilePath
userConfigDir = fromMaybe (error errorMsg) <$> result
  where
    chooseNonEmpty accA xA = do
        a <- accA
        if a == mempty
            then xA
            else pure a
    errorMsg = "Cannot set config location. Neither XDG_CONFIG_HOME nor HOME values were set."
    result =
        foldr
            chooseNonEmpty
            mempty
            [lookupEnv "XDG_CONFIG_HOME", fmap (fmap (<> "/.config")) (lookupEnv "HOME")]

data AppConfig = AppConfig
    { getInterpreterPrompt :: !T.Text
    -- ^ Prompt to show for the live interpreter.
    , getDebugConsoleOnStart :: !Bool
    -- ^ Display the debug console on start up.
    , getDebugLogPath :: !FilePath
    , getVerbosity :: !Int
    -- ^ Verbosity level.
    , getStartupSplashPath :: !(Maybe FilePath)
    , getCmd :: !T.Text
    -- ^ Command to run to initialise the interpreter.
    , getStartupCommands :: ![T.Text]
    -- ^ Commands to run in ghci during start up.
    }
    deriving (Show)

-- | Set up the default config for the App startup.
defaultConfig :: AppConfig
defaultConfig =
    AppConfig
        { getInterpreterPrompt = "ghci> "
        , getDebugConsoleOnStart = False
        , getDebugLogPath = ""
        , getVerbosity = 0
        , getStartupSplashPath = Nothing
        , getCmd = "cabal v2-repl --repl-options='-fno-it'"
        , getStartupCommands = mempty
        }

-- | Return the startup screen splash as an IsString.
loadStartupSplash :: (IsString s) => AppConfig -> IO (Maybe s)
loadStartupSplash _ = pure (pure SplashTextEmbed.splashText)
