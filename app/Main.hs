{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Data.Version
import qualified Paths_ghcitui as CabalPkg

import Control.Applicative (many)
import qualified Data.Text as T
import qualified Options.Applicative as Opt

import qualified Ghcitui.Brick as GB

-- | Holds passed in command line options.
data CmdOptions = CmdOptions
    { version :: !Bool
    , debugConsole :: !Bool
    , verbosity :: !Int
    , debugLogPath :: !FilePath
    , cmd :: !T.Text
    , workdir :: !FilePath
    -- ^ Launch the TUI at this work directory.
    , target :: !T.Text
    -- ^ Build target, passed as the final argument to cmd.
    }
    deriving (Show, Eq)

parseOpts :: Opt.Parser CmdOptions
parseOpts = do
    version <- Opt.switch (Opt.long "version" <> Opt.help "Print the version number and exit")
    debugConsole <-
        Opt.switch
            ( Opt.long "debug-console"
                <> Opt.help "Display the debug console"
            )
    verbosity <-
        length <$> many (Opt.flag' () (Opt.short 'v' <> Opt.help verbosityHelp))
    debugLogPath <-
        Opt.strOption
            ( Opt.long "daemon-log"
                <> Opt.help daemonLogHelp
                <> Opt.metavar "LOGFILE"
                <> Opt.value "stderr"
            )
    cmd <-
        Opt.strOption
            ( Opt.long "cmd"
                <> Opt.short 'c'
                <> Opt.metavar "CMD"
                <> Opt.help "Command to start the internal interpreter"
                <> Opt.value ""
            )
    workdir <-
        Opt.strOption
            ( Opt.long "workdir"
                <> Opt.short 'C'
                <> Opt.metavar "DIR"
                <> Opt.help "Set working dir"
                <> Opt.value ""
            )
    target <- Opt.argument Opt.str (Opt.metavar "TARGET" <> Opt.value "")
    pure CmdOptions{..}
  where
    verbosityHelp =
        "Set verbosity for output logs."
            <> " Pass multiple times (e.g -vvv) to increase the logging."
            <> " Use --daemon-log to specify where the logs go."
    daemonLogHelp =
        "File path for debugging daemon logs."
            <> " Used with -v."
            <> " Setting this to 'stdout' or 'stderr' sends logs to each, respectively."
            <> " Defaults to 'stderr'"

-- | The cabal package version.
programVersion :: String
programVersion = Data.Version.showVersion CabalPkg.version

main :: IO ()
main = do
    opts <- Opt.execParser parserInfo
    if version opts
        then do
            putStrLn $ programName <> " " <> programVersion
        else do
            let conf =
                    GB.defaultConfig
                        { GB.getDebugConsoleOnStart = debugConsole opts
                        , GB.getVerbosity = verbosity opts
                        , GB.getDebugLogPath = debugLogPath opts
                        , GB.getCmd =
                            if T.null $ cmd opts
                                then GB.getCmd GB.defaultConfig
                                else cmd opts
                        }
            GB.launchBrick conf (target opts) (workdir opts)
  where
    programName = "ghcitui"
    programDescription = Opt.progDesc (programName <> ": A TUI interface for GHCi")
    parserInfo = Opt.info (Opt.helper <*> parseOpts) (Opt.fullDesc <> programDescription)
