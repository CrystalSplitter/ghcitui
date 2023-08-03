module ParseContext
    ( ParseContextOut (..)
    , NameBinding (..)
    , BindingValue (..)
    , linesToText
    , parseContext
    , parseBreakResponse
    , parseBindings
    , parseShowBreaks
    , cleanResponse
    ) where

import Prelude hiding (lines)

import Data.Array ((!))
import Data.Functor ((<&>))
import Data.Maybe (isJust)
import Data.Text (Text, append, dropWhileEnd, lines, null, pack, strip, stripStart, unpack)
import Safe (atMay, headMay, lastDef, readMay, readNote)
import Text.Regex.TDFA (MatchResult (..), (=~~))

import qualified Loc
import NameBinding
import StringUtil

ghcidPrompt :: Text
ghcidPrompt = "#~GHCID-START~#"

linesToText :: [String] -> Text
linesToText = pack . unlines

-- | Output record datatype for @parseContext@.
data ParseContextOut = ParseContextOut
    { func :: !(Maybe Text)
    , filepath :: !(Maybe FilePath)
    , lineno :: !(Maybe Int)
    , colrange :: !(Maybe Int, Maybe Int)
    }
    deriving (Show)

-- | Parse the output from ":show context" for the interpreter state.
parseContext :: Text -> ParseContextOut
parseContext contextText =
    let splits = splitBy ghcidPrompt contextText
        stopReg :: Text -> [Text]
        stopReg s = s =~~ ("[ \t^]Stopped in ([[:alnum:]_.]+),.*" :: Text)

        -- Context line to actually parse
        infoLine :: Maybe Text
        infoLine =
            stripStart
                <$> foldr
                    (\next acc -> if isJust next then next else acc)
                    Nothing
                    (headMay . stopReg <$> splits)

        -- Function name, possibly preceded by module.
        myFunc :: Maybe Text
        myFunc = infoLine >>= (\x -> splitBy " " x `atMay` 2) <&> dropWhileEnd (`elem` [',', ' '])

        -- File path
        myFile :: Maybe FilePath
        myFile =
            infoLine
                >>= headMay . splitBy ":"
                >>= (\x -> splitBy ", " x `atMay` 1)
                <&> unpack

        -- Line number
        myLineno :: Maybe Int
        myLineno = infoLine >>= (\x -> splitBy ":" x `atMay` 1) <&> read . unpack

        -- Column range
        myColRange :: (Maybe Int, Maybe Int)
        myColRange =
            let colField = infoLine >>= (\x -> splitBy ":" x `atMay` 2)
                -- Parse the column field entries out.
                getCol :: Int -> Maybe Int
                getCol idx =
                    colField
                        >>= (\x -> splitBy "-" x `atMay` idx)
                        >>= readMay . unpack
             in (getCol 0, getCol 1)
     in ParseContextOut myFunc myFile myLineno myColRange

parseBreakResponse :: Text -> Either Text [Loc.ModuleLoc]
parseBreakResponse t
    | Just xs <- mapM matching (lines t) =
        let
            parseEach :: MatchResult Text -> Loc.ModuleLoc
            parseEach mr =
                let path = Just $ mr.mrSubs ! 2
                    lineno' = readMay $ unpack $ mr.mrSubs ! 3
                    colStart = readMay $ unpack $ mr.mrSubs ! 4
                    colEnd = readMay $ unpack $ mr.mrSubs ! 5
                 in Loc.ModuleLoc path lineno' (colStart, colEnd)
         in
            Right $ parseEach <$> xs
    | otherwise = Left t
  where
    breakpointReg =
        "Breakpoint (.*) activated at (.*):([0-9]*):([0-9]*)(-[0-9]*)?" :: Text
    matching :: Text -> Maybe (MatchResult Text)
    matching = (=~~ breakpointReg)

-- | Parse the output from ":show breaks"
parseShowBreaks :: Text -> Either Text [(Int, Loc.ModuleLoc)]
parseShowBreaks t
    | Just xs <- (mapM matching . lines) response = traverse parseEach xs
    | response == "No active breakpoints." = Right mempty
    | otherwise = Left (pack ("Response was" ++ show response))
  where
    response = strip t
    breakpointReg =
        "\\[([0-9]+)\\] +(.*) +([^:]*):([0-9]+):([0-9]+)(-[0-9]+)? ?(.*)" :: Text

    matching :: Text -> Maybe (MatchResult Text)
    matching = (=~~ breakpointReg)

    parseEach :: MatchResult Text -> Either Text (Int, Loc.ModuleLoc)
    parseEach mr =
        let 
            -- Don't need to use readMay because regex.
            idx = readNote "failed to read index." $ unpack $ mr.mrSubs ! 1 
            module_ = Just $ mr.mrSubs ! 2
            _filepath = Just $ mr.mrSubs ! 3 -- Not used currently but could be useful?
            lineno' = readMay $ unpack $ mr.mrSubs ! 4
            colStart = readMay $ unpack $ mr.mrSubs ! 5
            colEnd = readMay $ unpack $ mr.mrSubs ! 6
            enabled = case mr.mrSubs ! 7 of
                "enabled" -> Right True
                "disabled" -> Right False
                x -> Left ("Breakpoint neither enabled nor disabled: " `append` x)
         in enabled >> Right (idx, Loc.ModuleLoc module_ lineno' (colStart, colEnd))

-- | Parse the output of ":show modules".
parseShowModules :: Text -> Either Text [(Text, FilePath)]
parseShowModules t
    | Data.Text.null stripped = Right []
    | Just xs <- (mapM matching . lines) =<< response =
        let
            parseEach :: MatchResult Text -> (Text, FilePath)
            parseEach mr = (mr.mrSubs ! 1, unpack $ mr.mrSubs ! 2)
         in
            Right $ parseEach <$> xs
    | otherwise = Left ("failed to parse ':show modules': " `append` stripped)
  where
    stripped = strip t
    response = headMay (splitBy ghcidPrompt stripped)
    reg = "([[:alnum:]_.]+)[ \\t]+\\( *(.*),.*\\)" :: Text
    matching :: Text -> Maybe (MatchResult Text)
    matching = (=~~ reg)

-- | Parse the output of ":show bindings".
parseBindings :: Text -> Either Text [NameBinding Text]
parseBindings t
    | Data.Text.null stripped = Right []
    | Just xs <- mapM (=~~ reg) (lines stripped) =
        let
            parseEach :: MatchResult Text -> NameBinding Text
            parseEach mr = NameBinding (mr.mrSubs ! 1) (mr.mrSubs ! 2) (Evald $ mr.mrSubs ! 3)
         in
            Right $ parseEach <$> xs
    | otherwise = Left ("failed to parse ':show bindings': " `append` stripped)
  where
    stripped = strip t
    {- They look like...
        ghci> :show bindings
        _result :: Int = _
        it :: () = ()
    -}
    reg = "([a-z_][[:alnum:]_.']*) +:: +(.*) += +(.*)" :: Text

{- | Clean up GHCID exec returned messages/feedback.

Frequently, "exec" may include various GHCID prompts in its
returned messages. Return only the last prompt output, which seems to
include what we want fairly consistently.

Additionally, pack the lines into a single Text block.
-}
cleanResponse :: [String] -> Text
cleanResponse msgs = lastDef "" (splitBy ghcidPrompt (linesToText msgs))
