{-# LANGUAGE QuasiQuotes #-}

module Ghcitui.Ghcid.ParseTabCompletions (ParseError (..), parseCompletionsWithHeader) where

import Control.Error (readMay)
import Data.Array ((!))
import Data.String.Interpolate (i)
import qualified Data.Text as T
import Text.Regex.TDFA (MatchResult (..), (=~~))

import Ghcitui.Ghcid.ParseError (ParseError (..)) -- Re-export.

{- | Parse a completion result which begins with a header.

    Example input:
    [ "4 4 \"hello \""
    , "\"world\""
    , "\"wyvern\""
    , "\"withers\""
    , "\"wonderbolts\""]

    See https://downloads.haskell.org/ghc/latest/docs/users_guide/ghci.html#ghci-cmd-:complete
-}
parseCompletionsWithHeader
    :: [T.Text]
    -- ^ Full :complete output to parse.
    -> Either ParseError (T.Text, [T.Text])
    -- ^ Failure message (Left) or Completion possibilities (Right)
parseCompletionsWithHeader (headerLine : rest) = do
    sharedPrefix <- eSharedPrefix
    completions <- parseCompletions rest
    pure (sharedPrefix, completions)
  where
    eSharedPrefix = case (headerLine =~~ reg :: Maybe (MatchResult T.Text)) of
        Just match -> Right (mrSubs match ! 1)
        Nothing -> Left $ ParseError [i|Failed to parse ':complete' header line: '#{headerLine}'|]
    reg = ".* \"(.*)\"$" :: T.Text
parseCompletionsWithHeader _ = Left $ ParseError "Failed to parse completions with no header line"

parseCompletions
    :: [T.Text]
    -- ^ Completion lines.
    -> Either ParseError [T.Text]
    -- ^ Completion possibilities.
parseCompletions = mapM mapper
  where
    mapper x =
        maybe
            (Left $ ParseError [i|Failed to parse ':completion' entry '#{x}'|])
            (Right . T.pack)
            (readMay . T.unpack $ x)
