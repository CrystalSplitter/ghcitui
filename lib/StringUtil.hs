{-# LANGUAGE OverloadedStrings #-}

module StringUtil (splitBy) where

import Data.Text (Text, breakOn, drop, length, pack)
import Prelude hiding (drop, length)

splitBy :: Text -> Text -> [Text]
splitBy delim source =
    case breakOn delim source of
        (l, "") -> [l]
        (l, r) -> l : splitBy delim (drop (length delim) r)