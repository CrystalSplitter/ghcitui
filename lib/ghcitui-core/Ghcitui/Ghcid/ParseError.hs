module Ghcitui.Ghcid.ParseError (ParseError (..)) where

import qualified Data.Text as T

-- | Type to describe parsing errors.
newtype ParseError = ParseError T.Text deriving (Show, Eq)
