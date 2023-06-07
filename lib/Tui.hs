{-# LANGUAGE OverloadedStrings #-}

module Tui (loadFileSrc, getSurroundingSrc) where

{-

This file probably either doesn't need to exist, or should be merged with
BrickUI.hs

-}

import qualified Data.IORef as IORef
import Data.Text (Text, append, lines)
import Data.Text.IO (readFile)
import Prelude hiding (lines, readFile)

loadFileSrc :: FilePath -> IO (IORef.IORef Text)
loadFileSrc fp = do
    txt <- readFile fp
    IORef.newIORef txt

getSurroundingSrc
    :: Text
    -- ^ Source code as single Text
    -> Int
    -- ^ Window y size.
    -> Int
    -- ^ Cursor location
    -> [Text]
    -- ^ Window lines
getSurroundingSrc fileContents ySize location =
    let
        addMarker :: Int -> [Text] -> [Text]
        addMarker loc = zipWith (\idx val -> if idx == loc then "| > " `append` val else "|   " `append` val) [0 ..]
        loc1 = location - 1
        splitLines = lines fileContents
        lineCount = length splitLines
        beforeLineCount = max 0 (loc1 - (ySize `div` 2))
        afterLineCount = min (lineCount - loc1) ySize
     in
        (take afterLineCount . drop beforeLineCount . addMarker loc1) splitLines

addMarker :: Int -> [Text] -> [Text]
addMarker loc = zipWith (\idx val -> if idx == loc then "  > " `append` val else "    " `append` val) [0 ..]