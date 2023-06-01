{-# LANGUAGE OverloadedStrings #-}

module ParseContext (ParseContextOut (..), linesToText, parseContext) where

import Data.Functor ((<&>))
import Data.Maybe (fromJust, isJust)
import Data.Text (Text, dropWhileEnd, pack, stripStart, unpack)
import Safe
import Text.Regex.TDFA ((=~~))

import StringUtil
import Text.Read (readMaybe)

import Debug.Trace

ghcidPrompt :: Text
ghcidPrompt = "#~GHCID-START~#"

linesToText :: [String] -> Text
linesToText = pack . unlines

data ParseContextOut = ParseContextOut
    { func :: Maybe Text
    , filepath :: Maybe FilePath
    , lineno :: Maybe Int
    , colrange :: (Maybe Int, Maybe Int)
    }
    deriving (Show)

parseContext :: Text -> ParseContextOut
parseContext contextText =
    let splits = splitBy ghcidPrompt contextText
        stopReg :: Text -> [Text]
        stopReg s = s =~~ ("[ \t^]Stopped in ([[:alnum:]_.]+),.*" :: Text)
        infoLine :: Maybe Text
        infoLine =
            stripStart
                <$> foldr
                    (\next acc -> if isJust next then next else acc)
                    Nothing
                    (headMay . stopReg <$> splits)
        myFunc :: Maybe Text
        myFunc = infoLine >>= (\x -> splitBy " " x `atMay` 2) <&> dropWhileEnd (`elem` [',', ' '])
        myFile :: Maybe FilePath
        myFile =
            infoLine
                >>= headMay . splitBy ":"
                >>= (\x -> splitBy ", " x `atMay` 1)
                <&> unpack
        myLineno :: Maybe Int
        myLineno = infoLine >>= (\x -> splitBy ":" x `atMay` 1) <&> read . unpack
        myColRange :: (Maybe Int, Maybe Int)
        myColRange =
            let colField = infoLine >>= (\x -> splitBy ":" x `atMay` 2)
                -- | Parse the column field entries out.
                getCol :: Int -> Maybe Int
                getCol idx =
                    colField
                        >>= (\x -> splitBy "-" x `atMay` idx)
                        >>= readMaybe . unpack
             in trace (unpack $ fromJust infoLine) (getCol 0, getCol 1)
     in ParseContextOut myFunc myFile myLineno myColRange