{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Ghcitui.Brick.BrickUI
    ( launchBrick
    , AppState (..)
    ) where

import qualified Brick as B
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as B
import Brick.Widgets.Core ((<+>), (<=>))
import qualified Brick.Widgets.Dialog as B
import qualified Brick.Widgets.Edit as BE
import Control.Error (headMay)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Graphics.Vty as V
import Lens.Micro ((&), (^.))
import qualified Text.Wrap as Wrap

import qualified Ghcitui.Brick.AppConfig as AppConfig
import qualified Ghcitui.Brick.AppInterpState as AIS
import Ghcitui.Brick.AppState
    ( ActiveWindow (..)
    , AppState (..)
    , appInterpState
    , liveEditor
    , makeInitialState
    )
import qualified Ghcitui.Brick.AppState as AppState
import Ghcitui.Brick.AppTopLevel (AppName (..))
import qualified Ghcitui.Brick.DrawSourceViewer as DrawSourceViewer
import qualified Ghcitui.Brick.Events as Events
import qualified Ghcitui.Brick.HelpText as HelpText
import qualified Ghcitui.Brick.SourceWindow as SourceWindow
import qualified Ghcitui.Ghcid.Daemon as Daemon
import qualified Ghcitui.Loc as Loc
import qualified Ghcitui.NameBinding as NameBinding
import qualified Ghcitui.Util as Util

-- | Alias for 'AppState AppName' convenience.
type AppS = AppState AppName

appDraw :: AppS -> [B.Widget AppName]
appDraw s =
    [ drawDialogLayer s
    , drawBaseLayer s
    ]

dialogMaxWidth :: (Integral a) => a
dialogMaxWidth = 94

maxSourceFilePathWidth :: (Integral a) => a
maxSourceFilePathWidth = 45

{- | Draw the dialog layer.

     If there's no dialog, returns an 'emptyWidget'.
-}
drawDialogLayer :: AppS -> B.Widget AppName
-- Quit Dialog
drawDialogLayer AppState{activeWindow = ActiveDialogQuit} =
    B.withAttr (B.attrName "dialog") $ B.renderDialog dialogObj body
  where
    dialogObj = B.dialog (Just titleW) Nothing dialogMaxWidth
    titleW = B.txt "Please don't go. The drones need you. They look up to you."
    body =
        B.hCenter
            (B.padAll 1 (B.txt "Do you want to halt the current program and quit?"))
            <=> B.hCenter (B.padAll 1 (B.txt "[Enter] -> QUIT" <=> B.txt "[Esc/q] -> Go back"))
-- Help Dialog
drawDialogLayer AppState{activeWindow = ActiveDialogHelp} =
    B.withAttr (B.attrName "dialog") $ B.renderDialog dialogObj body
  where
    dialogObj = B.dialog (Just titleW) Nothing dialogMaxWidth
    titleW = B.txt "Actually reading the manual, huh?"
    body =
        ( B.hCenter
            . B.withVScrollBars B.OnRight
            . B.viewport HelpViewport B.Vertical
            $ B.padAll 1 (B.txt HelpText.helpText)
        )
            <=> ( B.hCenter
                    . B.padAll 1
                    $ B.txt "[Esc/Enter/q] -> Go back"
                )
-- No Dialog
drawDialogLayer _ = B.emptyWidget

drawBaseLayer :: AppS -> B.Widget AppName
drawBaseLayer s =
    (sourceWindowBox <=> interpreterBox <=> debugBox) <+> infoBox s
  where
    sourceLabel =
        markLabel
            (s.activeWindow == ActiveCodeViewport)
            ( "Source: "
                <> maybe
                    "?"
                    (Util.dropMiddleToFitText maxSourceFilePathWidth . T.pack)
                    (AppState.selectedFile s)
            )
            "[Esc]"
    interpreterLabel =
        markLabel
            (s.activeWindow == ActiveLiveInterpreter)
            ( if s ^. appInterpState . AIS.viewLock
                then "GHCi"
                else "GHCi (Scrolling)"
            )
            "[Ctrl+x]"

    -- For seeing the source code.
    sourceWindowBox :: B.Widget AppName
    sourceWindowBox =
        B.borderWithLabel sourceLabel
            . appendLastCommand
            . B.padRight B.Max
            . B.padBottom B.Max
            $ DrawSourceViewer.drawSourceViewer s
      where
        appendLastCommand w =
            B.padBottom B.Max (w <=> B.hBorder <=> (lastCmdWidget <+> lineNumRatioWidget))
          where
            selectedLine = AppState.selectedLine s
            totalLines = s ^. AppState.sourceWindow & SourceWindow.srcWindowLength
            percentageNum =
                if totalLines > 0
                    then (selectedLine * 100) `div` totalLines
                    else 0
            lineNumRatioWidget =
                B.txt
                    ( Util.showT selectedLine
                        <> "/"
                        <> Util.showT totalLines
                        <> "L ("
                        <> Util.showT percentageNum
                        <> "%)"
                    )
            lastCmdWidget =
                B.padRight
                    B.Max
                    ( case headMay (Daemon.execHist (AppState.interpState s)) of
                        Just h -> B.txt h
                        _ -> B.txt " "
                    )

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
            B.txt (AppConfig.getInterpreterPrompt . AppState.appConfig $ s)
                <+> BE.renderEditor displayF enableCursor (s ^. liveEditor)
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
                    applyVisTo (x : xs) = B.visible x : xs
                    applyVisTo [] = []
                 in B.borderWithLabel (B.txt "Debug")
                        . B.vLimit 10
                        . B.withVScrollBars B.OnRight
                        . B.viewport DebugPanel B.Vertical
                        . B.padRight B.Max
                        . B.vBox
                        . reverse
                        . applyVisTo
                        $ (B.txt <$> logDisplay)
            else B.emptyWidget

-- | Draw the info panel.
infoBox :: AppS -> B.Widget AppName
infoBox appState =
    B.borderWithLabel infoLabel
        . B.hLimit (AppState.getInfoWidth appState)
        . B.padRight B.Max
        . B.padBottom B.Max
        $ bindingBox
            <=> B.hBorderWithLabel modulesLabel
            <=> moduleBox appState
            <=> B.hBorderWithLabel (B.txt "Trace History")
            <=> drawTraceBox appState
  where
    isActive = activeWindow appState == ActiveInfoWindow
    infoLabel = B.txt "Info"
    modulesLabel =
        markLabel
            isActive
            "Modules"
            (if activeWindow appState /= ActiveLiveInterpreter then "[M]" else mempty)
    intState = interpState appState

    bindingBox :: B.Widget AppName
    bindingBox = B.viewport BindingViewport B.Vertical contents
      where
        contents = case NameBinding.renderNamesTxt <$> Daemon.bindings intState of
            Left _ -> B.txt "<Error displaying bindings>"
            Right [] -> B.txt " " -- Can't be an empty widget due to padding?
            Right bs -> B.vBox (B.txtWrapWith wrapSettings <$> bs)
        wrapSettings =
            Wrap.defaultWrapSettings
                { Wrap.preserveIndentation = True
                , Wrap.breakLongWords = True
                , Wrap.fillStrategy = Wrap.FillIndent 2
                }

moduleBox :: AppS -> B.Widget AppName
moduleBox appState =
    B.cached ModulesViewport $
        if null mfmAssocs
            then B.hCenter $ B.txt "<No module mappings>"
            else
                B.withVScrollBars B.OnRight
                    . B.viewport ModulesViewport B.Vertical
                    $ B.vBox moduleEntries
  where
    mfmAssocs = Loc.moduleFileMapAssocs (Daemon.moduleFileMap (AppState.interpState appState))
    moduleEntries = zipWith mkModEntryWidget [0 ..] mfmAssocs

    mkModEntryWidget :: Int -> (T.Text, FilePath) -> B.Widget n
    mkModEntryWidget idx (modName, fp) =
        if isSelected && isActive
            then
                B.visible
                    ( B.withAttr
                        (B.attrName "selected-marker")
                        (B.txt cursor <+> B.txtWrapWith wrapSettings entryText)
                    )
            else B.txt padding <+> B.txt entryText
      where
        isSelected = AppState.getSelectedModuleInInfoPanel appState == idx
        isActive = AppState.activeWindow appState == ActiveInfoWindow
        entryText = modName <> " = " <> T.pack fp
        padding = "  "
        cursor = "> "
        wrapSettings =
            Wrap.defaultWrapSettings
                { Wrap.preserveIndentation = True
                , Wrap.breakLongWords = True
                , Wrap.fillStrategy = Wrap.FillIndent 2
                }

-- | Draw the trace box in the info panel.
drawTraceBox :: AppState AppName -> B.Widget AppName
drawTraceBox s = contents
  where
    contents =
        if null traceHist
            then B.txt "<No trace>"
            else B.vBox $ B.txt <$> traceHist
    traceHist :: [T.Text]
    traceHist = Daemon.traceHist (AppState.interpState s)

-- | Mark the label if the first arg is True.
markLabel
    :: Bool
    -- ^ Conditional to mark with.
    -> T.Text
    -- ^ Text to use for the label.
    -> T.Text
    -- ^ Addendum unfocused text.
    -> B.Widget a
markLabel False labelTxt focus = B.txt . appendFocusButton $ labelTxt
  where
    appendFocusButton t = if focus == mempty then t else t <> " " <> focus
markLabel True labelTxt _ =
    B.withAttr (B.attrName "highlight") (B.txt ("#> " <> labelTxt <> " <#"))

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
                    , (B.attrName "dialog", B.style V.standout)
                    ]
        }

-- | Start the Brick UI
launchBrick :: AppConfig.AppConfig -> T.Text -> FilePath -> IO ()
launchBrick conf target cwd = do
    T.putStrLn $ "Starting up GHCiTUI with: `" <> AppConfig.getCmd conf <> "`..."
    T.putStrLn "This can take a while..."
    initialState <- makeInitialState conf target cwd
    _ <- B.defaultMain brickApp initialState
    T.putStrLn "GHCiTUI has shut down; have a nice day :)"
    pure ()
