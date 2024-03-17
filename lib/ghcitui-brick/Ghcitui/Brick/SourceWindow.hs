{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Ghcitui.Brick.SourceWindow
    ( SourceWindow (srcElements)

      -- * Creation
    , mkSourcWindow

      -- * Rendering
    , renderSourceWindow

      -- * Event Handling
    , ScrollDir (..)
    , scrollTo
    , srcWindowScrollPage
    , srcWindowMoveSelectionBy
    , srcWindowReplace
    , setSelectionTo
    , updateVerticalSpace

      -- * Lenses
    , srcElementsL
    , srcNameL
    , srcSelectedLineL
    , srcWindowStartL
    , srcWindowVerticalSpaceL

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

import qualified Ghcitui.Util as Util

-- | Hold data regarding a code source viewing window.
data SourceWindow name elem = SourceWindow
    { srcElements :: !(Vec.Vector elem)
    -- ^ The actual entries for each source window.
    , srcWindowStart :: !Int
    -- ^ The starting position of the window, as a line number (1-indexed).
    -- No lines before this line number is rendered.
    , srcWindowVerticalSpace :: !(Maybe Int)
    -- ^ The maximum amount of visible lines at any point in time.
    , srcName :: !name
    -- ^ The name of the window.
    , srcSelectedLine :: !(Maybe Int)
    -- ^ The currently selected line in the window.
    }
    deriving (Show)

makeLensesFor
    [ ("srcElements", "srcElementsL")
    , ("srcWindowStart", "srcWindowStartL")
    , ("srcWindowVerticalSpace", "srcWindowVerticalSpaceL")
    , ("srcName", "srcNameL")
    , ("srcSelectedLine", "srcSelectedLineL")
    ]
    ''SourceWindow

-- | The difference between the last rendered line and the first rendered line.
srcWindowLineDiffCount :: SourceWindow name elem -> Maybe Int
srcWindowLineDiffCount SourceWindow{srcWindowVerticalSpace = Just sWVS} = pure $ sWVS - 1
srcWindowLineDiffCount _ = Nothing

-- | The line number of the last viewable line in the window.
getLastRenderedLine :: SourceWindow name elem -> Maybe Int
getLastRenderedLine srcW@SourceWindow{srcWindowStart} = do
    diffCount <- srcWindowLineDiffCount srcW
    pure $ diffCount + srcWindowStart

-- | Render a 'SourceWindow' into a Brick 'B.Widget'.
renderSourceWindow
    :: (Ord n)
    => (Int -> Bool -> e -> B.Widget n)
    -- ^ Render function.
    -> SourceWindow n e
    -- ^ 'SourceWindow' to render.
    -> B.Widget n
    -- ^ The newly created widget.
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

{- | Return the length of the full contents of the source code stored in the window.

     Note, does NOT return the current length/height/size of the rendered widget.
-}
srcWindowLength :: SourceWindow n e -> Int
srcWindowLength = Vec.length . srcElements

{- | Set the source window end line inside of the given 'EventM' Monad.
     This is primarily for internal consistency, and is cheap. It should be called any time
     the srcWindowStart changes.
-}
updateVerticalSpace :: (Ord n) => SourceWindow n e -> B.EventM n m (SourceWindow n e)
updateVerticalSpace srcW@SourceWindow{srcName {- , srcContainerName -}} = do
    mSrcNameExtent <- B.lookupExtent srcName
    let mSpace = case mSrcNameExtent of
            Just extent ->
                Just . snd . B.extentSize $ extent
            _ -> Nothing
    pure (Lens.set srcWindowVerticalSpaceL mSpace srcW)

-- | Scroll to a given position, and move the source line along the way if needed.
scrollTo :: Int -> SourceWindow n e -> SourceWindow n e
scrollTo pos srcW@SourceWindow{srcWindowVerticalSpace = Just vSpace} =
    srcW{srcWindowStart = clampedPos, srcSelectedLine = newSelection}
  where
    -- Clamp between start line and one window away from the end.
    clampedPos = Util.clamp (1, srcWindowLength srcW - vSpace) pos

    newSelection
        | -- Choose the starting line if we're trying to go past the beginning.
          isScrollingPastStart =
            Just 1
        | -- Choose the last line if we're trying to go past the end.
          isScrollingPastEnd =
            Just $ srcWindowLength srcW
        | otherwise = newClampedSelectedLine
    isScrollingPastStart = pos < 1
    isScrollingPastEnd = pos >= srcWindowLength srcW -- Using >= because of a hack.
    newClampedSelectedLine :: Maybe Int
    newClampedSelectedLine = do
        ssl <- srcSelectedLine srcW
        diffCount <- srcWindowLineDiffCount srcW
        pure $ Util.clamp (clampedPos, clampedPos + diffCount) ssl
scrollTo _ srcW = srcW

-- | Direction to scroll by.
data ScrollDir = Up | Down deriving (Eq, Show)

-- | Scroll by a full page in a direction.
srcWindowScrollPage :: (Ord n) => ScrollDir -> SourceWindow n e -> B.EventM n m (SourceWindow n e)
srcWindowScrollPage dir srcW = srcWindowScrollPage' dir <$> updateVerticalSpace srcW

srcWindowScrollPage' :: ScrollDir -> SourceWindow n e -> SourceWindow n e
srcWindowScrollPage' dir srcW@SourceWindow{srcWindowStart} =
    case dir of
        Up -> scrollTo onePageUpPos srcW
        Down -> scrollTo (fromMaybe srcWindowStart (getLastRenderedLine srcW)) srcW
  where
    onePageUpPos = srcWindowStart - vSpace + 1 -- Plus one to preserve the top line.
    vSpace = fromMaybe 0 (srcWindowVerticalSpace srcW)

-- | Set the selection to a given position, and scroll the window accordingly.
setSelectionTo
    :: (Ord n)
    => Int
    -- ^ Line number to set the selection to (1-indexed)
    -> SourceWindow n e
    -- ^ Source window to update.
    -> B.EventM n m (SourceWindow n e)
setSelectionTo pos srcW = do
    srcW' <- updateVerticalSpace srcW
    case (getLastRenderedLine srcW', srcSelectedLine srcW') of
        (Just end, Just oldSelectedLine) -> do
            let delta = pos - oldSelectedLine
            if pos < srcWindowStart srcW' || pos > end
                then srcWindowMoveSelectionBy delta srcW
                else do
                    pure $ srcW{srcSelectedLine = Just pos}
        _ -> setSelectionToFallback pos srcW'

-- | Fallback function for setting the source window selection line, when we can't set it properly.
setSelectionToFallback :: Int -> SourceWindow name elem -> B.EventM name m (SourceWindow name elem)
setSelectionToFallback pos srcW = pure $ srcW{srcSelectedLine = Just pos, srcWindowStart = pos}

-- | Move the selected line by a given amount.
srcWindowMoveSelectionBy
    :: (Ord n)
    => Int
    -- ^ Delta to move the selected line.
    -> SourceWindow n e
    -- ^ Source window to update.
    -> B.EventM n m (SourceWindow n e)
srcWindowMoveSelectionBy amnt sw = do
    srcW <- updateVerticalSpace sw
    case (getLastRenderedLine srcW, srcWindowLineDiffCount srcW, srcSelectedLine srcW) of
        (Just end, Just renderHeight, Just oldSLine)
            | newSLine < srcWindowStart srcW ->
                pure $ scrollTo newSLine srcW{srcSelectedLine = Just newSLine}
            | newSLine > end ->
                pure $ scrollTo (newSLine - renderHeight) srcW{srcSelectedLine = Just newSLine}
            | otherwise -> pure $ srcW{srcSelectedLine = Just newSLine}
          where
            newSLine = Util.clamp (1, Vec.length (srcElements srcW)) $ oldSLine + amnt
        _ -> pure srcW

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
    -> SourceWindow n T.Text
mkSourcWindow sourceWindowName text =
    SourceWindow
        { srcElements = lineVec
        , srcWindowStart = 1
        , srcSelectedLine = Just 1
        , srcName = sourceWindowName
        , srcWindowVerticalSpace = Nothing
        }
  where
    lineVec = Vec.fromList (T.lines text)
