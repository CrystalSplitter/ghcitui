{-# LANGUAGE OverloadedStrings #-}

module AppConfig where

import Data.Maybe
import Data.Text (Text)
import System.Environment (lookupEnv)

userConfigDir :: IO FilePath
userConfigDir = fromMaybe (error errorMsg) <$> result
  where
    innerLift accA xA = do
        a <- accA
        if a == mempty
            then xA
            else pure a
    errorMsg = "Cannot set config location. Neither XDG_CONFIG_HOME nor HOME values were set."
    result =
        foldr
            innerLift
            mempty
            [lookupEnv "XDG_CONFIG_HOME", fmap (fmap (<> "/.config")) (lookupEnv "HOME")]

defaultSplashPath :: IO FilePath
defaultSplashPath = fmap (<> "/ghcitui/assets/splash") userConfigDir

data AppConfig = AppConfig
    { getInterpreterPrompt :: !Text
    -- ^ Prompt to show for the live interpreter.
    , getDebugConsoleOnStart :: !Bool
    -- ^ Display the debug console on start up.
    , getStartupSplashPath :: !(Maybe FilePath)
    , getCmd :: !Text
    , getStartupCommands :: ![Text]
    }

defaultConfig :: AppConfig
defaultConfig =
    AppConfig
        { getInterpreterPrompt = "ghci> "
        , getDebugConsoleOnStart = False
        , getStartupSplashPath = Nothing
        , getCmd = "cabal v2-repl"
        , getStartupCommands = mempty
        }

resolveStartupSplashPath :: AppConfig -> IO FilePath
resolveStartupSplashPath config = maybe defaultSplashPath pure $ getStartupSplashPath config
