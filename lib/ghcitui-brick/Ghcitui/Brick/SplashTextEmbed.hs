{-# LANGUAGE TemplateHaskell #-}

module Ghcitui.Brick.SplashTextEmbed (splashText) where

import Data.String (IsString)

import qualified Data.FileEmbed as FileEmbed

splashText :: (IsString a) => a
splashText = $(FileEmbed.makeRelativeToProject "assets/splash.txt" >>= FileEmbed.embedStringFile)
