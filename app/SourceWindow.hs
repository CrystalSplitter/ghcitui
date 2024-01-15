{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module SourceWindow
    ( SourceWindow (srcElements)

      -- * Creation
    , mkSourcWindow

      -- * Rendering
    , renderSourceWindow

      -- * Event Handling
    , ScrollDir (..)
    , scrollTo
    , srcWindowScrollPage
    , updateSrcWindowEnd
    , srcWindowMoveSelectionBy
    , srcWindowReplace
    , setSelectionTo

      -- * Lenses
    , srcElementsL
    , srcNameL
    , srcSelectedLineL
    , srcWindowStartL

    -- * Misc
    , srcWindowLength
    ) where

import qualified Brick as B
import Control.Error (fromMaybe)
import qualified Data.Text as T
import qualified Data.Vector as Vec
import Lens.Micro ((^.))
import qualified Lens.Micro as Lens
import Lens.Micro.TH (makeLensesFor)

import qualified Util

data SourceWindow n e = SourceWindow
    { srcElements :: !(Vec.Vector e)
    , srcWindowStart :: !Int
    , srcWindowEnd :: !(Maybe Int)
    , srcName :: !n
    , srcSelectedLine :: !(Maybe Int)
    }
    deriving (Show)

makeLensesFor
    [ ("srcElements", "srcElementsL")
    , ("srcWindowStart", "srcWindowStartL")
    , ("srcWindowEnd", "srcWindowEndL")
    , ("srcName", "srcNameL")
    , ("srcSelectedLine", "srcSelectedLineL")
    ]
    ''SourceWindow

renderSourceWindow
    :: (Ord n)
    => (Int -> Bool -> e -> B.Widget n)
    -> SourceWindow n e
    -> B.Widget n
renderSourceWindow func srcW = B.reportExtent (srcName srcW) (B.Widget B.Greedy B.Greedy renderM)
  where
    renderM = do
        c <- B.getContext
        let availableHeight = c ^. B.availHeightL + 1
        let renderHeight = Util.clamp (1, remainingElements) availableHeight
        let slicedElems = Vec.slice startZeroIdx renderHeight elems
        let drawnElems =
                [ func idx (Just idx == srcSelectedLine srcW) e
                | (idx, e) <- zip [srcWindowStart srcW ..] . Vec.toList $ slicedElems
                ]
        let trailingSpaces = availableHeight - length drawnElems
        -- This is a fairly weird list comprehension, since it either has only one element
        -- or none. But it works, and is for some reason recommended by hlint. Ugh.
        let trailingSpaceWidgets = [B.txt (T.replicate trailingSpaces "\n") | trailingSpaces > 0]
        B.render
            . B.vBox
            $ drawnElems <> trailingSpaceWidgets
    startZeroIdx = Util.clamp (0, srcWindowLength srcW - 1) $ srcWindowStart srcW - 1
    remainingElements = srcWindowLength srcW - startZeroIdx
    elems = srcElements srcW

srcWindowLength :: SourceWindow n e -> Int
srcWindowLength = Vec.length . srcElements

-- | Set the source window end line inside of the given 'EventM' Monad.
updateSrcWindowEnd :: (Ord n) => SourceWindow n e -> B.EventM n m (SourceWindow n e)
updateSrcWindowEnd srcW@SourceWindow{srcWindowStart, srcName} = do
    mExtent <- B.lookupExtent srcName
    let end = case mExtent of
            Just extent ->
                -- -1 offset since the end is inclusive.
                Just $ (snd . B.extentSize $ extent) + srcWindowStart - 1
            _ -> Nothing
    pure (Lens.set srcWindowEndL end srcW)

-- | Scroll to a given position, and move the source line along the way if needed.
scrollTo :: Int -> SourceWindow n e -> SourceWindow n e
scrollTo pos srcW@SourceWindow{srcWindowEnd = Just windowEnd} =
    srcW{srcWindowStart = clampedPos, srcSelectedLine = newSelection}
  where
    clampedPos = Util.clamp (1, srcWindowLength srcW - renderHeight) pos
    newSelection
        | -- Choose the starting line if we're trying to go past the beginning.
          isScrollingPastStart =
            Just 1
        | -- Choose the last line if we're trying to go past the end.
          isScrollingPastEnd =
            Just $ srcWindowLength srcW
        | otherwise = newClampedSelectedLine
    renderHeight = windowEnd - srcWindowStart srcW
    isScrollingPastStart = pos < 1
    isScrollingPastEnd = pos >= srcWindowLength srcW -- Using >= because of a hack.
    newClampedSelectedLine =
        Util.clamp
            (clampedPos, clampedPos + renderHeight)
            <$> srcSelectedLine srcW
scrollTo _ srcW = srcW

data ScrollDir = Up | Down deriving (Eq, Show)

-- | Scroll by a full page in a direction.
srcWindowScrollPage :: (Ord n) => ScrollDir -> SourceWindow n e -> B.EventM n m (SourceWindow n e)
srcWindowScrollPage dir srcW = srcWindowScrollPage' dir <$> updateSrcWindowEnd srcW

-- | Internal helper.
srcWindowScrollPage' :: ScrollDir -> SourceWindow n e -> SourceWindow n e
srcWindowScrollPage' dir srcW =
    case dir of
        Up ->
            let renderHeight = windowEnd - srcWindowStart srcW
             in scrollTo (srcWindowStart srcW - renderHeight) srcW
        Down -> scrollTo windowEnd srcW
  where
    windowEnd = fromMaybe 1 $ srcWindowEnd srcW

setSelectionTo :: (Ord n) => Int -> SourceWindow n e -> B.EventM n m (SourceWindow n e)
setSelectionTo pos srcW@SourceWindow{srcSelectedLine = Just sl, srcWindowEnd = Just end} =
    if pos < srcWindowStart srcW || pos > end
        then srcWindowMoveSelectionBy delta srcW
        else do
            pure $ srcW{srcSelectedLine = Just pos}
  where
    delta = pos - sl
setSelectionTo _ srcW = pure srcW

-- | Move the selected line by a given amount.
srcWindowMoveSelectionBy :: (Ord n) => Int -> SourceWindow n e -> B.EventM n m (SourceWindow n e)
srcWindowMoveSelectionBy amnt sw = do
    srcW' <- updateSrcWindowEnd sw
    case srcWindowEnd srcW' of
        Just end -> do
            let start = srcWindowStart srcW'
            let mSLine = srcSelectedLine srcW'
            let renderHeight = end - start
            pure $ case mSLine of
                Just sLine
                    | newSLine < start ->
                        scrollTo newSLine srcW'{srcSelectedLine = Just newSLine}
                    | newSLine > end ->
                        scrollTo (newSLine - renderHeight) srcW'{srcSelectedLine = Just newSLine}
                    | otherwise -> srcW'{srcSelectedLine = Just newSLine}
                  where
                    newSLine = Util.clamp (1, Vec.length (srcElements srcW')) $ sLine + amnt
                _ -> srcW'
        Nothing -> pure srcW'

{- | Replace the contents of a given source window, and reset the pseudo-viewport's position
     to the top.
-}
srcWindowReplace :: (Foldable f) => f e -> SourceWindow n e -> SourceWindow n e
srcWindowReplace foldable srcW =
    srcW{srcSelectedLine = Just 1, srcWindowStart = 1, srcElements = elems}
  where
    elems = Vec.fromList . foldr (:) [] $ foldable

-- | Create a new source window from some text.
mkSourcWindow
    :: n
    -- ^ Name for the source window.
    -> T.Text
    -- ^ Text contents of the source window (to be split up).
    -> SourceWindow.SourceWindow n T.Text
mkSourcWindow name text =
    SourceWindow.SourceWindow
        { srcElements = lineVec
        , srcWindowStart = 1
        , srcSelectedLine = Just 1
        , srcName = name
        , srcWindowEnd = Nothing
        }
  where
    lineVec = Vec.fromList (T.lines text)