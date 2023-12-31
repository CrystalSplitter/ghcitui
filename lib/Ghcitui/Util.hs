module Ghcitui.Util (showT, splitBy, linesToText) where

import Data.Text (Text, breakOn, drop, length, pack)
import Prelude hiding (drop, length)

-- | Split text based on a delimiter.
splitBy
    :: Text
    -- ^ Delimeter.
    -> Text
    -- ^ Text to split on.
    -> [Text]
splitBy "" source = [source]
splitBy delim source =
    case breakOn delim source of
        (l, "") -> [l]
        (l, r) -> l : splitBy delim (drop (length delim) r)

-- | Convert Strings to Text.
linesToText :: [String] -> Text
linesToText = pack . Prelude.unlines

-- | 'show' but to Text.
showT :: (Show a) => a -> Text
showT = pack . show
