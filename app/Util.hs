module Util where

import Data.Text (Text, pack)

showT :: (Show a) => a -> Text
showT = Data.Text.pack . show

-- Return the number of digits in a given integral
getNumDigits :: (Integral a) => a -> Int
getNumDigits 0 = 1
getNumDigits num = truncate (logBase 10 (fromIntegral num) :: Double) + 1

-- | Format digits into a string with padding.
formatDigits
    :: Int
    -- ^ Number of spaces
    -> Int
    -- ^ Number to format digits of
    -> Text
    -- ^ Formatted Text
formatDigits spacing num = pack (replicate amount ' ') <> pack (show num)
  where
    amount = spacing - getNumDigits num

clamp
    :: (Ord a)
    => (a, a)
    -- ^ The minimum and maximum (inclusive)
    -> a
    -- ^ Value to clamp
    -> a
    -- ^ Result
clamp (mi, mx) v
    | v < mi = mi
    | v > mx = mx
    | otherwise = v
