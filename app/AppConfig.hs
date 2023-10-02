{-# LANGUAGE OverloadedStrings #-}

module AppConfig where

import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import System.Environment (lookupEnv)

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

defaultSplashPath :: IO FilePath
defaultSplashPath = fmap (<> "/ghcitui/assets/splash") userConfigDir

data AppConfig = AppConfig
    { getInterpreterPrompt :: !T.Text
    -- ^ Prompt to show for the live interpreter.
    , getDebugConsoleOnStart :: !Bool
    -- ^ Display the debug console on start up.
    , getStartupSplashPath :: !(Maybe FilePath)
    , getCmd :: !T.Text
    -- ^ Command to run to initialise the interpreter.
    , getStartupCommands :: ![T.Text]
    -- ^ Commands to run in ghci during start up.
    }

defaultConfig :: AppConfig
defaultConfig =
    AppConfig
        { getInterpreterPrompt = "ghci> "
        , getDebugConsoleOnStart = False
        , getStartupSplashPath = Nothing
        , getCmd = "cabal v2-repl --repl-options='-fno-it'"
        , getStartupCommands = mempty
        }

resolveStartupSplashPath :: AppConfig -> IO FilePath
resolveStartupSplashPath config = maybe defaultSplashPath pure $ getStartupSplashPath config
