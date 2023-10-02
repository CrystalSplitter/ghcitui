module Main where

import qualified Data.Text as T
import qualified Options.Applicative as Opt

import qualified AppConfig
import BrickUI (launchBrick)

{-
Old code for reference.

launch :: IO ()
launch = do
    state <- D.startup "cabal repl" "."
    (state, _) <- D.exec state ":l app/Main.hs"
    fileRef <- loadFileSrc "app/Main.hs"
    let surroundingSrc windowSize D.InterpState{D.lineno} =
            do
                src <- readIORef fileRef
                case lineno of
                    Nothing -> pure []
                    Just l -> pure $ getSurroundingSrc src windowSize l
    state <- D.stepInto state "fibty 10"
    let loop s = do
            print s
            newWindow <- surroundingSrc 5 s
            newS <- D.step state
            mapM_ TextIO.putStrLn newWindow
            putStr "%% "
            SIO.hFlush SIO.stdout
            stdinLine <- getLine
            if stdinLine == "q"
                then pure ()
                else do
                    (newS, msgs) <- D.exec state stdinLine
                    mapM_ (putStrLn . ("OUT: " ++)) msgs
                    loop newS
    loop state
    D.quit state
    pure ()
-}

-- | Holds passed in command line options.
data CmdOptions = CmdOptions
    { debugConsole :: !Bool
    , cmd :: !T.Text
    , workdir :: !FilePath
    -- ^ Launch the TUI at this work directory.
    , target :: !T.Text
    -- ^ Build target, passed as the final argument to cmd.
    }
    deriving (Show, Eq)

parseOpts :: Opt.Parser CmdOptions
parseOpts =
    CmdOptions
        <$> Opt.switch
            ( Opt.long "debug-console"
                <> Opt.short 'd'
                <> Opt.help "Display the debug console"
            )
        <*> Opt.strOption
            ( Opt.long "cmd"
                <> Opt.short 'c'
                <> Opt.metavar "CMD"
                <> Opt.help "Command to start the internal interpreter."
                <> Opt.value ""
            )
        <*> Opt.strOption
            ( Opt.long "workdir"
                <> Opt.short 'C'
                <> Opt.metavar "DIR"
                <> Opt.help "Set working dir."
                <> Opt.value ""
            )
        <*> Opt.argument Opt.str (Opt.metavar "TARGET")

main :: IO ()
main = do
    opts <- Opt.execParser parserInfo
    let defConf = AppConfig.defaultConfig
    let conf =
            defConf
                { AppConfig.getDebugConsoleOnStart = debugConsole opts
                , AppConfig.getCmd =
                    if T.null $ cmd opts
                        then AppConfig.getCmd defConf
                        else cmd opts
                }
    launchBrick conf (target opts) (workdir opts)
  where
    parserInfo =
        Opt.info
            (Opt.helper Opt.<*> parseOpts)
            (Opt.fullDesc <> Opt.progDesc "Program Description")
