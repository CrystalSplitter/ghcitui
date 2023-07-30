module StringUtil (splitBy) where

import Data.Text (Text, breakOn, drop, length, unlines, unsnoc, length)
import Prelude hiding (drop, length, unlines)

-- | Split text based on a delimiter.
splitBy
    :: Text
    -- ^ Delimeter.
    -> Text
    -- ^ Text to split on.
    -> [Text]
splitBy delim source =
    case breakOn delim source of
        (l, "") -> [l]
        (l, r) -> l : splitBy delim (drop (length delim) r)

-- | Split text based on a predicate.
-- splitByPred
--     :: (Text -> Bool)
--     -- ^ Predicate
--     -> Text
--     -- ^ Text to split on.
--     -> [Text]
-- splitByPred _ "" = []
-- splitByPred pr toSplit = : splitByPred (restOf
--   where
--     restOf x = maybe mempty snd (uncons x)

-- | Cleans prompt data from the GHCID output.
cleanPrompt
    :: Text
    -- ^ Prompt text to remove.
    -> [Text]
    -- ^ Input lines to clean.
    -> [Text]
    -- ^ Cleaned output
cleanPrompt prompt ls =
    let sp = splitBy prompt (unlines ls)
        fileRangeOf t =
            maybe (0,0) (\(_, lastChar) ->
                if lastChar == ']'
                    then (0, length t - 1)
                    else (0, 0)
            ) (unsnoc t)
     in sp
