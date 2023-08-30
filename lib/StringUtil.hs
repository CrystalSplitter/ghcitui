module StringUtil (linesToText, splitBy) where

import Data.Text (pack, Text, breakOn, drop, length)
import Prelude hiding (drop, length)

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

-- | Convert Strings to Text.
linesToText :: [String] -> Text
linesToText = pack . Prelude.unlines
