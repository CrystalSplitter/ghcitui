{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module Ghcitui.Ghcid.ParseContext
    ( ParseContextOut (..)
    , ParseContextReturn (..)
    , NameBinding (..)
    , BindingValue (..)
    , parseContext
    , parseBreakResponse
    , parseBindings
    , parseShowBreaks
    , parseShowModules
    , isHistoryFailureMsg
    , cleanResponse
    , ParseError (..)
    ) where

import Prelude hiding (lines)

import Control.Applicative ((<|>))
import Control.Error
import Data.Array ((!))
import Data.String.Interpolate (i)
import qualified Data.Text as T
import Text.Regex.TDFA (MatchResult (..), (=~~))
import qualified Text.Regex.TDFA as Regex

import Ghcitui.Ghcid.ParseError (ParseError (..)) -- Re-export.
import qualified Ghcitui.Loc as Loc
import Ghcitui.NameBinding
import Ghcitui.Util

ghcidPrompt :: T.Text
ghcidPrompt = "#~GHCID-START~#"

-- | Output record datatype for 'parseContext'.
data ParseContextOut = ParseContextOut
    { func :: !T.Text
    , filepath :: !FilePath
    , pcSourceRange :: !Loc.SourceRange
    }
    deriving (Eq, Show)

data ParseContextReturn = PCError ParseError | PCNoContext | PCContext ParseContextOut deriving (Eq, Show)

-- | Parse the output from ":show context" for the interpreter state.
parseContext :: T.Text -> ParseContextReturn
parseContext contextText =
    case eInfoLine contextText of
        Right (func, rest) ->
            let sourceRange = parseSourceRange rest
             in case parseFile rest of
                    Right f -> PCContext (ParseContextOut func f sourceRange)
                    Left e -> PCError e
        Left (ParseError e) ->
            let contextTextLines = T.lines contextText
             in if all (`elem` ["", "()"]) contextTextLines
                    then PCNoContext
                    else PCError (ParseError [i|parsing context: #{e}|])

parseFile :: T.Text -> Either ParseError FilePath
parseFile s
    | Just mr <- s =~~ ("^[ \t]*([^:]*):" :: T.Text) = Right (T.unpack (mrSubs mr ! 1))
    | otherwise = Left (ParseError [i|Could not parse file from: '#{s}'|])

-- | Parse a source range structure into a SourceRange object.
parseSourceRange :: T.Text -> Loc.SourceRange
parseSourceRange s
    -- Matches (12,34)-(56,78)
    | Just mr <- matches "\\(([0-9]+),([0-9]+)\\)-\\(([0-9]+),([0-9]+)\\)" = fullRange mr
    -- Matches 12:34-56
    | Just mr <- matches "([0-9]+):([0-9]+)-([0-9]+)" = lineColRange mr
    -- Matches 12:34
    | Just mr <- matches "([0-9]+):([0-9]+)" = lineColSingle mr
    -- Matches 12
    | Just mr <- matches "([0-9]+)" = onlyLine mr
    | otherwise = Loc.unknownSourceRange
  where
    matches :: T.Text -> Maybe (MatchResult T.Text)
    matches reg = s =~~ reg

    unpackRead :: (Read a) => MatchResult T.Text -> Int -> Maybe a
    unpackRead mr idx = readMay (T.unpack (mrSubs mr ! idx))

    fullRange :: MatchResult T.Text -> Loc.SourceRange
    fullRange mr =
        let startLine = unpackRead mr 1
            startCol = unpackRead mr 2
            endLine = unpackRead mr 3
            endCol = unpackRead mr 4
         in Loc.SourceRange{startLine, startCol, endLine, endCol}

    lineColRange :: MatchResult T.Text -> Loc.SourceRange
    lineColRange mr =
        let startLine = unpackRead mr 1
            startCol = unpackRead mr 2
            endLine = startLine
            endCol = unpackRead mr 3
         in Loc.SourceRange{startLine, startCol, endLine, endCol}

    lineColSingle :: MatchResult T.Text -> Loc.SourceRange
    lineColSingle mr =
        let startLine = unpackRead mr 1
            startCol = unpackRead mr 2
            endLine = startLine
            endCol = fmap (+ 1) startCol
         in Loc.SourceRange{startLine, startCol, endLine, endCol}

    onlyLine :: MatchResult T.Text -> Loc.SourceRange
    onlyLine mr =
        let startLine = unpackRead mr 1
            startCol = Nothing
            endLine = startLine
            endCol = Nothing
         in Loc.SourceRange{startLine, startCol, endLine, endCol}

{- | Converts a multiline contextText from:

        Stopped in Foo.Bar, other stuff here
        more stuff that doesn't match
        even more stuff

    into ("Foo.Bar", "other stuff here") if the text matches.
-}
eInfoLine :: T.Text -> Either ParseError (T.Text, T.Text)
eInfoLine "" = Left $ ParseError "Could not find info line in empty string"
eInfoLine contextText =
    note
        (ParseError [i|Could not match info line: '#{showT splits}'|])
        mStopLine
  where
    splits = splitBy ghcidPrompt contextText
    mStopLine = (\mr -> (mrSubs mr ! 1, mrSubs mr ! 2)) <$> mStopLineMatchRes
    mStopLineMatchRes = foldr (\n acc -> acc <|> stopReg n) Nothing splits
    -- Match on the "Stopped in ..." line.
    stopReg :: T.Text -> Maybe (MatchResult T.Text)
    stopReg s = s =~~ ("^[ \t]*Stopped in ([[:alnum:]_.()]+'*),(.*)" :: T.Text)

parseBreakResponse :: T.Text -> Either T.Text [Loc.ModuleLoc]
parseBreakResponse t
    | Just xs <- mapM matching (T.lines t) =
        let
            parseEach :: MatchResult T.Text -> Loc.ModuleLoc
            parseEach mr =
                let moduleName = mr.mrSubs ! 2
                    startLine = readMay $ T.unpack $ mr.mrSubs ! 3
                    endLine = startLine
                    startCol = readMay $ T.unpack $ mr.mrSubs ! 4
                    endCol = readMay $ T.unpack $ mr.mrSubs ! 5
                 in Loc.ModuleLoc moduleName Loc.SourceRange{startLine, startCol, endLine, endCol}
         in
            Right $ parseEach <$> xs
    | otherwise = Left ("Could not parse breakpoint from: " <> t)
  where
    breakpointReg =
        "Breakpoint (.*) activated at (.*):([0-9]*):([0-9]*)(-[0-9]*)?" :: T.Text
    matching :: T.Text -> Maybe (MatchResult T.Text)
    matching = (=~~ breakpointReg)

-- | Parse the output from ":show breaks"
parseShowBreaks
    :: T.Text
    -- ^ Message to parse.
    -> Either T.Text [(Int, Loc.ModuleLoc)]
    -- ^ Tuples are (breakpoint index, location).
parseShowBreaks t
    | Just xs <- (mapM matching . T.lines) response = traverse parseEach xs
    | response == "No active breakpoints." = Right mempty
    | otherwise = Left (T.pack ("Response was" ++ show response))
  where
    response = T.strip t
    breakpointReg =
        "\\[([0-9]+)\\] +(.*) +([^:]*):(.*) +([a-zA-Z_-]+)" :: T.Text

    matching :: T.Text -> Maybe (MatchResult T.Text)
    matching = (=~~ breakpointReg)

    parseEach :: MatchResult T.Text -> Either T.Text (Int, Loc.ModuleLoc)
    parseEach mr =
        let
            -- Don't need to use readMay because regex.
            eIdx = readErr "Failed to read index" $ T.unpack $ mr.mrSubs ! 1
            module_ = mr.mrSubs ! 2
            _filepath = Just $ mr.mrSubs ! 3 -- Not used currently but could be useful?
            sourceRange = parseSourceRange $ mr.mrSubs ! 4
            enabled = case mr.mrSubs ! 5 of
                "enabled" -> Right True
                "disabled" -> Right False
                x -> Left ("Breakpoint neither enabled nor disabled: " <> x)
         in
            case (sourceRange, eIdx) of
                (_, Right idx)
                    | sourceRange == Loc.unknownSourceRange ->
                        Left ("Could not parse source range for breakpoint " <> showT idx)
                    | otherwise ->
                        enabled >> Right (idx, Loc.ModuleLoc module_ sourceRange)
                (_, Left e) -> Left e

-- | Parse the output of ":show modules".
parseShowModules :: T.Text -> Either ParseError [(T.Text, FilePath)]
parseShowModules t
    | T.null stripped = Right []
    | Just xs <- matchingLines =
        let
            parseEach :: MatchResult T.Text -> (T.Text, FilePath)
            parseEach mr = (mr.mrSubs ! 1, T.unpack $ mr.mrSubs ! 2)
         in
            Right $ parseEach <$> xs
    | otherwise = Left (ParseError [i|Failed to parse ':show modules': #{stripped}|])
  where
    stripped = T.strip t
    matchingLines = mapMaybe matching . T.lines <$> lastMay (splitBy ghcidPrompt stripped)
    reg = "([[:alnum:]_.]+)[ \\t]+\\( *([^,]*),.*\\)" :: T.Text
    matching :: T.Text -> Maybe (MatchResult T.Text)
    matching = (=~~ reg)

-- Sometimes there's lines that are just Unit '()'. Unsure
-- what they are meant to represent in the binding list.
dropUnitLines :: [T.Text] -> [T.Text]
dropUnitLines = filter (\x -> T.strip x /= "()")

-- | Parse the output of ":show bindings".
parseBindings :: T.Text -> Either T.Text [NameBinding T.Text]
parseBindings t
    | T.null stripped = Right []
    | Just xs <- mapM (=~~ reg) (dropUnitLines (mergeBindingLines (T.lines stripped))) =
        let
            parseEach :: MatchResult T.Text -> NameBinding T.Text
            parseEach mr = NameBinding (mr.mrSubs ! 1) (mr.mrSubs ! 2) (Evald $ mr.mrSubs ! 3)
         in
            Right $ parseEach <$> xs
    | otherwise = Left ("Failed to parse ':show bindings':\n" <> stripped)
  where
    stripped = T.strip t

    mergeBindingLines :: [T.Text] -> [T.Text]
    mergeBindingLines [] = []
    mergeBindingLines [x] = [x]
    mergeBindingLines (x1 : x2 : xs) =
        case T.uncons x2 of
            Just (' ', rest) ->
                let newLine = (T.strip x1 <> " " <> T.strip rest)
                 in mergeBindingLines (newLine : xs)
            _ -> x1 : mergeBindingLines (x2 : xs)

    {- They look like...
        ghci> :show bindings
        somethingLong ::
          [AVeryLong
            SubType
             -> SomeResultType] = value
        _result :: Int = _
        it :: () = ()
    -}
    reg = "([a-z_][[:alnum:]_.']*) +:: +(.*) += +(.*)" :: T.Text

-- | Whether a given text line represents a failed history lookup.
isHistoryFailureMsg :: T.Text -> Bool
isHistoryFailureMsg text = (reg `Regex.match` text) || (reg2 `Regex.match` text)
  where
    execOption = Regex.ExecOption False
    compOption = Regex.defaultCompOpt{Regex.caseSensitive = False}
    makeRegex :: T.Text -> Regex.Regex
    makeRegex = Regex.makeRegexOpts compOption execOption
    reg = makeRegex "not +stopped +at +a +breakpoint"
    reg2 = makeRegex "empty history(\\.)?"

{- | Clean up GHCID exec returned messages/feedback.

Frequently, "exec" may include various GHCID prompts in its
returned messages. Return only the last prompt output, which seems to
include what we want fairly consistently.

Additionally, pack the lines into a single T.Text block.
-}
cleanResponse :: [T.Text] -> T.Text
cleanResponse =
    T.unlines
        . dropUnitLines
        . T.lines
        . lastDef ""
        . splitBy ghcidPrompt
        . T.unlines
