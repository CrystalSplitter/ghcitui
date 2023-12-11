{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module BrickUI
    ( launchBrick
    , AppState (..)
    ) where

import qualified Brick as B
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as B
import Brick.Widgets.Core ((<+>), (<=>))
import qualified Brick.Widgets.Edit as BE
import Control.Error (headMay)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Graphics.Vty as V
import Lens.Micro ((&), (<&>), (^.))
import qualified Text.Wrap as Wrap

import qualified AppConfig
import qualified AppInterpState as AIS
import AppState
    ( ActiveWindow (..)
    , AppState (..)
    , appInterpState
    , getSourceContents
    , liveEditor'
    , makeInitialState
    )
import qualified AppState
import AppTopLevel (AppName (..))
import qualified Events
import qualified Ghcid.Daemon as Daemon
import qualified Loc
import qualified NameBinding
import qualified Util

-- | Alias for 'AppState AppName' convenience.
type AppS = AppState AppName

appDraw :: AppS -> [B.Widget AppName]
appDraw s =
    [ (viewportBox <=> interpreterBox <=> debugBox)
        -- TODO: Make this an expandable viewport, maybe?
        <+> infoBox s
    ]
  where
    sourceLabel =
        markLabel
            (s.activeWindow == ActiveCodeViewport)
            ( "Source: " <> maybe "?" T.pack s.selectedFile
            )
    interpreterLabel =
        markLabel
            (s.activeWindow == ActiveLiveInterpreter)
            ( if s ^. appInterpState . AIS.viewLock
                then "Interpreter"
                else "Interpreter (Scrolling)"
            )

    -- For seeing the source code.
    viewportBox :: B.Widget AppName
    viewportBox =
        B.borderWithLabel sourceLabel
            . appendLastCommand
            . B.withVScrollBars B.OnRight
            . B.viewport CodeViewport B.Vertical
            . B.padRight B.Max
            $ codeViewportDraw s
      where
        appendLastCommand w =
            case headMay s.interpState.execHist of
                Just h -> B.padBottom B.Max (w <=> B.hBorder <=> B.txt h)
                _ -> w

    -- For the REPL.
    interpreterBox :: B.Widget AppName
    interpreterBox =
        B.borderWithLabel interpreterLabel
            . B.vLimit (AppState.getReplHeight s)
            . B.withVScrollBars B.OnRight
            . B.viewport LiveInterpreterViewport B.Vertical
            $ previousOutput <=> lockToBottomOnViewLock promptLine
      where
        enableCursor = True
        previousOutput =
            if null s.interpLogs
                then B.emptyWidget
                else
                    B.txt
                        . T.unlines
                        . reverse
                        $ s.interpLogs
        promptLine :: B.Widget AppName
        promptLine =
            B.txt s.appConfig.getInterpreterPrompt
                <+> BE.renderEditor displayF enableCursor (s ^. liveEditor')
          where
            displayF :: [T.Text] -> B.Widget AppName
            displayF t = B.vBox $ B.txt <$> t
        lockToBottomOnViewLock w =
            if s ^. appInterpState . AIS.viewLock
                then B.visible w
                else w

    debugBox =
        if s.displayDebugConsoleLogs
            then
                let logDisplay =
                        if null s.debugConsoleLogs then [" "] else s.debugConsoleLogs
                 in B.borderWithLabel (B.txt "Debug")
                        . B.withVScrollBars B.OnRight
                        . B.padRight B.Max
                        . B.txt
                        . T.unlines
                        . reverse
                        $ logDisplay
            else B.emptyWidget

infoBox :: AppS -> B.Widget AppName
infoBox appState =
    B.borderWithLabel (B.txt "Info")
        . B.hLimit (AppState.getInfoWidth appState)
        . B.padRight B.Max
        . B.padBottom B.Max
        $ bindingBox
            <=> B.hBorderWithLabel (B.txt "Modules")
            <=> moduleBox
            <=> B.hBorderWithLabel (B.txt "Trace History")
            <=> traceBox
  where
    wrapSettings =
        Wrap.defaultWrapSettings
            { Wrap.preserveIndentation = True
            , Wrap.breakLongWords = True
            , Wrap.fillStrategy = Wrap.FillIndent 2
            }
    intState = interpState appState

    bindingBox :: B.Widget AppName
    bindingBox = case NameBinding.renderNamesTxt <$> Daemon.bindings intState of
        Left _ -> B.txt "<Error displaying bindings>"
        Right [] -> B.txt " " -- Can't be an empty widget due to padding?
        Right bs -> B.vBox (B.txtWrapWith wrapSettings <$> bs)

    moduleBox :: B.Widget AppName
    moduleBox =
        if null mfmAssocs
            then B.txt "<No module mappings>"
            else foldr1 (<=>) (mkModEntryWidget <$> mfmAssocs)
      where
        mfmAssocs = Loc.moduleFileMapAssocs (Daemon.moduleFileMap intState)
        mkModEntryWidget (modName, fp) = B.txt (modName <> " > " <> T.pack fp)

    traceBox :: B.Widget AppName
    traceBox =
        if null traceHist
            then B.txt "<No trace>"
            else B.vBox $ B.txt <$> traceHist
      where
        traceHist :: [T.Text]
        traceHist = Daemon.traceHist intState

-- | Mark the label if the first arg is True.
markLabel :: Bool -> T.Text -> B.Widget a
markLabel False labelTxt = B.txt (labelTxt <> " [Ctrl+x]")
markLabel True labelTxt =
    B.withAttr (B.attrName "highlight") (B.txt ("#> " <> labelTxt <> " <#"))

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

-- | Make the primary viewport widget.
codeViewportDraw :: AppS -> B.Widget AppName
codeViewportDraw s =
    case (currentlyRunning, sourceDataMaybe) of
        (_, Just sourceData) -> codeViewportDraw' s sourceData
        (False, _) -> notRunningWidget
        (_, Nothing) -> noSourceWidget
  where
    currentlyRunning = Daemon.isExecuting (interpState s)
    sourceDataMaybe = getSourceContents s
    padWidget w =
        B.padTop (B.Pad 3)
            . B.hCenter
            $ B.withAttr (B.attrName "styled") w
    splashWidget = maybe (B.txt "No splash file loaded.") B.txt s.splashContents
    notRunningWidget =
        padWidget splashWidget
            <=> padWidget (B.txt "Nothing executing. Maybe run something?")
    noSourceWidget = padWidget splashWidget <=> padWidget (B.txt "Can't display. Source not found.")

-- | Viewport when we have source contents.
codeViewportDraw' :: AppS -> T.Text -> B.Widget AppName
codeViewportDraw' s sourceData = composedTogether
  where
    -- Source data split on lines.
    splitSourceData = T.lines sourceData
    -- Source data split on lines, but only the bit we may want to render.
    windowedSplitSourceData :: [(Int, T.Text)]
    windowedSplitSourceData =
        withLineNums splitSourceData
      where
        -- TODO: Maybe figure this bit out for performance?
        -- We probably can use vpContentSize somehow to ensure
        -- that we don't bother looking at source beyond
        -- the render window.
        -- _loadedWindowSize = error "loadedWindowSize not implemented"
        startLineno = 1
        withLineNums = zip [startLineno ..]

    breakpoints :: [Int]
    breakpoints = maybe mempty (\f -> Daemon.getBpInFile f (interpState s)) (selectedFile s)

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
            , gutterDigitWidth = Util.getNumDigits $ length splitSourceData
            , isSelected = lineno == s.selectedLine
            }

    prefixLineDefault' :: (Int, B.Widget n) -> B.Widget n
    prefixLineDefault' (lineno, w) =
        prependGutter
            (gutterInfoForLine lineno)
            w

    originalLookupLineNo :: Int
    originalLookupLineNo =
        s.interpState.pauseLoc
            >>= Loc.startLine . Loc.sourceRange
            & fromMaybe 0

    stoppedLineW :: T.Text -> B.Widget AppName
    stoppedLineW lineTxt =
        let Loc.SourceRange{startCol, endCol} =
                maybe Loc.unknownSourceRange Loc.sourceRange (Daemon.pauseLoc (interpState s))
            lineWidget = makeStoppedLineWidget lineTxt (startCol, endCol)
         in prefixLineDefault' (originalLookupLineNo, lineWidget)

    stoppedRangeW :: Int -> T.Text -> B.Widget AppName
    stoppedRangeW lineno lineTxt =
        prefixLineDefault' (lineno, B.forceAttrAllowStyle (B.attrName "stop-line") (B.txt lineTxt))

    selectedLineW :: T.Text -> B.Widget AppName
    selectedLineW lineTxt =
        let transform = id -- Potentialy useful for highlighting spaces?
            lineWidget = B.txt $ transform lineTxt
         in prefixLineDefault' (s.selectedLine, lineWidget)

    -- Select which line widget we want to draw based on both the interpreter
    -- state and the app state.
    --
    -- It's important that the line information is cached, because
    -- each line is actually pretty expensive to render.
    composedTogetherHelper :: (Int, T.Text) -> B.Widget AppName
    composedTogetherHelper (lineno, lineTxt) = lineWidgetCached
      where
        sr = maybe Loc.unknownSourceRange Loc.sourceRange (Daemon.pauseLoc (interpState s))
        mLineno = Loc.singleify sr
        lineWidget = case mLineno of
            -- This only makes the stopped line widget appear for the start loc.
            Just (singleLine, _) | lineno == singleLine -> stoppedLineW lineTxt
            -- If it's a range, just try to show the range.
            _
                | Loc.isLineInside sr lineno -> stoppedRangeW lineno lineTxt
            -- If it's not something we stopped in, just show the selection normally.
            _
                | lineno == s.selectedLine -> selectedLineW lineTxt
            -- Default case.
            _ -> ((\w -> prefixLineDefault' (lineno, w)) . B.txt) lineTxt
        lineWidgetCached = B.cached (CodeViewportLine lineno) lineWidget

    composedTogether :: B.Widget AppName
    composedTogether = B.vBox (createWidget <$> windowedSplitSourceData)
      where
        wrapSelectedLine lineno w =
            if lineno == s.selectedLine
                then -- Add highlighting, then mark it as visible in the viewport.
                    B.visible $ B.modifyDefAttr (`V.withStyle` V.bold) w
                else w
        createWidget (num, lineTxt) = wrapSelectedLine num (composedTogetherHelper (num, lineTxt))

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

-- -------------------------------------------------------------------------------------------------
-- Brick Main
-- -------------------------------------------------------------------------------------------------

-- | Brick main program.
brickApp :: B.App AppS e AppName
brickApp =
    B.App
        { B.appDraw = appDraw
        , B.appChooseCursor = Events.handleCursorPosition
        , B.appHandleEvent = Events.handleEvent
        , B.appStartEvent = pure ()
        , B.appAttrMap =
            const $
                B.attrMap
                    V.defAttr
                    [ (B.attrName "stop-line", B.fg V.red)
                    , (B.attrName "line-numbers", B.fg V.cyan)
                    , (B.attrName "selected-line-numbers", B.fg V.yellow)
                    , (B.attrName "selected-line", B.bg V.brightBlack)
                    , (B.attrName "selected-marker", B.fg V.yellow)
                    , (B.attrName "breakpoint-marker", B.fg V.red)
                    , (B.attrName "underline", B.style V.underline)
                    , (B.attrName "styled", B.fg V.magenta `V.withStyle` V.bold)
                    , (B.attrName "highlight", B.style V.standout)
                    ]
        }

-- | Start the Brick UI
launchBrick :: AppConfig.AppConfig -> T.Text -> FilePath -> IO ()
launchBrick conf target cwd = do
    initialState <- makeInitialState conf target cwd
    _ <- B.defaultMain brickApp initialState
    pure ()
