{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Ghcitui.Brick.EventUtils
    ( shortenText
    , commonPrefixes
    , reflowText
    , invalidateLineCache
    ) where

import qualified Brick.Main as B
import qualified Brick.Types as B

import Data.List (foldl')
import qualified Data.Text as T

-- | Limit text to a given length, and cut with an elipses.
shortenText :: Int -> T.Text -> T.Text
shortenText maxLen text
    | len <= maxLen = text
    | otherwise = T.take (maxLen - 1) text <> "…"
  where
    len = T.length text

-- | Return the shared prefix among all the input Texts.
commonPrefixes :: [T.Text] -> T.Text
commonPrefixes [] = ""
commonPrefixes (t : ts) = foldl' folder t ts
  where
    folder :: T.Text -> T.Text -> T.Text
    folder acc t' = case T.commonPrefixes acc t' of
        Just (p, _, _) -> p
        _ -> ""

-- TODO: Invalidate only the lines instead of the entire application.
invalidateLineCache :: (Ord n) => B.EventM n (state n) ()
invalidateLineCache = B.invalidateCache

{- | Reflow entries of text into columns.
     Mostly useful right now for printing autocomplete suggestions into columns.
-}
reflowText
    :: Int
    -- ^ Num columns
    -> Int
    -- ^ Column width
    -> [T.Text]
    -- ^ Text entries to reflow
    -> [T.Text]
    -- ^ Reflowed lines.
reflowText numCols colWidth = go
  where
    go :: [T.Text] -> [T.Text]
    go [] = []
    go entries' = makeLine toMakeLine : go rest
      where
        (toMakeLine, rest) = splitAt numCols entries'
    maxTextLen = colWidth - 1
    makeLine xs = T.concat (T.justifyLeft colWidth ' ' . shortenText maxTextLen <$> xs)
