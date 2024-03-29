{-# LANGUAGE TemplateHaskell #-}

module Ghcitui.Brick.HelpText (helpText) where

import Data.String (IsString)

import qualified Data.FileEmbed as FileEmbed

helpText :: (IsString a) => a
helpText = $(FileEmbed.makeRelativeToProject "gen/MANUAL.txt" >>= FileEmbed.embedStringFile)
