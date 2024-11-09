{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Ghcitui.Brick.EventUtils
    ( shortenText
    , commonPrefixes
    , runDaemon
    , runDaemon2
    , reflowText
    , invalidateLineCache
    ) where

import qualified Brick.Main as B
import qualified Brick.Types as B
import Control.Monad.IO.Class (MonadIO (..))

import Data.List (foldl')
import qualified Data.Text as T
import qualified Ghcitui.Brick.AppState as AppState
import qualified Ghcitui.Ghcid.Daemon as Daemon

-- | Limit text to a given length, and cut with an elipses.
shortenText :: Int -> T.Text -> T.Text
shortenText maxLen text
    | len <= maxLen = text
    | otherwise = T.take (maxLen - 1) text <> "…"
  where
    len = T.length text

-- | Return the shared prefix among all the input Texts.
commonPrefixes :: [T.Text] -> T.Text
commonPrefixes [] = ""
commonPrefixes (t : ts) = foldl' folder t ts
  where
    folder :: T.Text -> T.Text -> T.Text
    folder acc t' = case T.commonPrefixes acc t' of
        Just (p, _, _) -> p
        _ -> ""

-- TODO: Invalidate only the lines instead of the entire application.
invalidateLineCache :: (Ord n) => B.EventM n (state n) ()
invalidateLineCache = B.invalidateCache

-- | Run a DaemonIO function on a given interpreter state, within an EventM monad.
runDaemon
    :: (Ord n)
    => (Daemon.InterpState () -> Daemon.DaemonIO (Daemon.InterpState ()))
    -> AppState.AppState n
    -> B.EventM n m (AppState.AppState n)
runDaemon f appState = do
    interp <- liftIO $ do
        (Daemon.run . f) appState.interpState >>= \case
            Right out -> pure out
            Left er -> error $ show er
    AppState.selectPausedLine appState{AppState.interpState = interp}

-- | Alternative to 'runDaemon' which returns a value along with the state.
runDaemon2
    :: (Ord n)
    => (Daemon.InterpState () -> Daemon.DaemonIO (Daemon.InterpState (), a))
    -> AppState.AppState n
    -> B.EventM n m (AppState.AppState n, a)
runDaemon2 f appState = do
    (interp, x) <-
        liftIO $
            (Daemon.run . f) appState.interpState >>= \case
                Right out -> pure out
                Left er -> error $ show er
    newState <- AppState.selectPausedLine appState{AppState.interpState = interp}
    pure (newState, x)

{- | Reflow entries of text into columns.
     Mostly useful right now for printing autocomplete suggestions into columns.
-}
reflowText
    :: Int
    -- ^ Num columns
    -> Int
    -- ^ Column width
    -> [T.Text]
    -- ^ Text entries to reflow
    -> [T.Text]
    -- ^ Reflowed lines.
reflowText numCols colWidth = go
  where
    go :: [T.Text] -> [T.Text]
    go [] = []
    go entries' = makeLine toMakeLine : go rest
      where
        (toMakeLine, rest) = splitAt numCols entries'
    maxTextLen = colWidth - 1
    makeLine xs = T.concat (T.justifyLeft colWidth ' ' . shortenText maxTextLen <$> xs)