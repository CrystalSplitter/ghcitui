{-# LANGUAGE NamedFieldPuns #-}

module DrawSourceViewer (drawSourceViewer) where

import qualified Brick as B
import qualified Brick.Widgets.Center as B
import Brick.Widgets.Core ((<+>), (<=>))
import Control.Error (fromMaybe)
import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Data.Text as T
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V
import Lens.Micro ((^.))

import AppState (AppState)
import qualified AppState
import AppTopLevel (AppName (..))
import qualified Ghcitui.Brick.SourceWindow as SourceWindow
import qualified Ghcitui.Ghcid.Daemon as Daemon
import qualified Ghcitui.Loc as Loc
import qualified Ghcitui.Util as Util

-- | Make the primary viewport widget.
drawSourceViewer :: AppState AppName -> B.Widget AppName
drawSourceViewer s
    | (srcWindow ^. SourceWindow.srcElementsL) /= mempty = drawSourceViewer' s srcWindow
    | not currentlyRunning = notRunningWidget
    | otherwise = noSourceWidget
  where
    currentlyRunning = Daemon.isExecuting (AppState.interpState s)
    srcWindow = s ^. AppState.sourceWindow
    notRunningWidget =
        padWidget splashWidget
            <=> padWidget (B.txt "Nothing executing. Maybe run something?")
    noSourceWidget =
        padWidget splashWidget <=> padWidget (B.txt "Can't display. Source not found.")
    padWidget w =
        B.padTop (B.Pad 3)
            . B.hCenter
            $ B.withAttr (B.attrName "styled") w
    splashWidget = maybe (B.txt "No splash file loaded.") B.txt s.splashContents

-- -------------------------------------------------------------------------------------------------
-- Source Viewer Drawing Details
-- -------------------------------------------------------------------------------------------------

-- | Information used to compute the gutter status of each line.
data GutterInfo = GutterInfo
    { isStoppedHere :: !Bool
    -- ^ Is the interpreter stopped/paused here?
    , isBreakpoint :: !Bool
    -- ^ Is there a breakpoint here?
    , isSelected :: !Bool
    -- ^ Is this line currently selected by the user?
    , gutterLineNumber :: !Int
    -- ^ What line number is this?
    , gutterDigitWidth :: !Int
    -- ^ How many columns is the gutter line number?
    }

-- | Prepend gutter information on each line in the primary viewport.
prependGutter :: GutterInfo -> B.Widget n -> B.Widget n
prependGutter gi line = makeGutter gi <+> line

{- | Create the gutter section for a given line (formed from GutterInfo).
     This should be cached wherever since there can be thousands of these
     in a source.
-}
makeGutter :: GutterInfo -> B.Widget n
makeGutter GutterInfo{..} =
    lineNoWidget <+> spaceW <+> stopColumn <+> breakColumn <+> spaceW
  where
    spaceW = B.txt " "
    lineNoWidget =
        let attr = B.attrName (if isSelected then "selected-line-numbers" else "line-numbers")
         in B.withAttr attr (B.txt (Util.formatDigits gutterDigitWidth gutterLineNumber))
    breakColumn
        | isSelected && isBreakpoint = B.withAttr (B.attrName "selected-marker") (B.txt "@")
        | isSelected = B.withAttr (B.attrName "selected-marker") (B.txt ">")
        | isBreakpoint = B.withAttr (B.attrName "breakpoint-marker") (B.txt "*")
        | otherwise = spaceW
    stopColumn
        | isStoppedHere = B.withAttr (B.attrName "stop-line") (B.txt "!")
        | otherwise = spaceW

-- | Panel when we have source contents.
drawSourceViewer'
    :: AppState AppName
    -> SourceWindow.SourceWindow AppName T.Text
    -> B.Widget AppName
drawSourceViewer' s sourceWindow = composedTogether
  where
    isSelectedLine :: Int -> Bool
    isSelectedLine lineno = Just lineno == sourceWindow ^. SourceWindow.srcSelectedLineL

    composedTogether :: B.Widget AppName
    composedTogether = SourceWindow.renderSourceWindow createWidget sourceWindow
      where
        createWidget lineno _old lineTxt =
            styliseLine $ composedTogetherHelper lineno lineTxt
          where
            styliseLine w =
                if isSelectedLine lineno
                    then -- Add highlighting, then mark it as visible in the viewport.
                        B.modifyDefAttr (`V.withStyle` V.bold) w
                    else w

    -- Select which line widget we want to draw based on both the interpreter
    -- state and the app state.
    --
    -- It's important that the line information is cached, because
    -- each line is actually pretty expensive to render.
    composedTogetherHelper :: Int -> T.Text -> B.Widget AppName
    composedTogetherHelper lineno lineTxt = lineWidgetCached
      where
        sr = maybe Loc.unknownSourceRange Loc.sourceRange (Daemon.pauseLoc (AppState.interpState s))
        mLineno = Loc.singleify sr
        lineWidget = case mLineno of
            -- This only makes the stopped line widget appear for the start loc.
            Just (singleLine, _) | lineno == singleLine -> stoppedLineW lineTxt
            -- If it's a range, just try to show the range.
            _
                | Loc.isLineInside sr lineno -> stoppedRangeW
            -- Default case (includes selected and non-selected).
            _ -> (\w -> prefixLine (lineno, w)) . B.txt $ lineTxt
        lineWidgetCached = B.cached (SourceWindowLine lineno) lineWidget

        stoppedRangeW :: B.Widget AppName
        stoppedRangeW =
            prefixLine
                ( lineno
                , B.forceAttrAllowStyle (B.attrName "stop-line") (B.txt lineTxt)
                )

    prefixLine :: (Int, B.Widget n) -> B.Widget n
    prefixLine (lineno', w) =
        prependGutter
            (gutterInfoForLine lineno')
            w
      where
        gutterInfoForLine :: Int -> GutterInfo
        gutterInfoForLine lineno =
            GutterInfo
                { isStoppedHere =
                    s.interpState.pauseLoc
                        <&> Loc.sourceRange
                        <&> (`Loc.isLineInside` lineno)
                        & fromMaybe False
                , isBreakpoint = lineno `elem` breakpoints
                , gutterLineNumber = lineno
                , gutterDigitWidth = Util.getNumDigits $ sourceWindowLength sourceWindow
                , isSelected = isSelectedLine lineno
                }
          where
            breakpoints :: [Int]
            breakpoints =
                maybe
                    mempty
                    (\f -> Daemon.getBpInFile f (AppState.interpState s))
                    (AppState.selectedFile s)

    originalLookupLineNo :: Int
    originalLookupLineNo =
        s.interpState.pauseLoc
            >>= Loc.startLine . Loc.sourceRange
            & fromMaybe 0

    stoppedLineW :: T.Text -> B.Widget AppName
    stoppedLineW lineTxt =
        let Loc.SourceRange{startCol, endCol} =
                maybe
                    Loc.unknownSourceRange
                    Loc.sourceRange
                    (Daemon.pauseLoc (AppState.interpState s))
            lineWidget = makeStoppedLineWidget lineTxt (startCol, endCol)
         in prefixLine (originalLookupLineNo, lineWidget)

sourceWindowLength :: SourceWindow.SourceWindow n e -> Int
sourceWindowLength = Vec.length . SourceWindow.srcElements

-- | Make the Stopped Line widget (the line where we paused execution)
makeStoppedLineWidget :: T.Text -> Loc.ColumnRange -> B.Widget AppName
makeStoppedLineWidget lineData (Nothing, _) =
    B.forceAttrAllowStyle (B.attrName "stop-line") (B.txt lineData)
makeStoppedLineWidget lineData (Just startCol, Nothing) =
    makeStoppedLineWidget lineData (Just startCol, Just (startCol + 1))
makeStoppedLineWidget lineData (Just startCol, Just endCol) =
    B.forceAttrAllowStyle
        (B.attrName "stop-line")
        ( B.txt lineDataBefore
            <+> B.withAttr (B.attrName "highlight") (B.txt lineDataRange)
            <+> B.txt lineDataAfter
        )
  where
    (lineDataBefore, partial) = T.splitAt (startCol - 1) lineData
    (lineDataRange, lineDataAfter) = T.splitAt (endCol - startCol + 1) partial
