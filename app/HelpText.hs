{-# LANGUAGE TemplateHaskell #-}

module HelpText where

import Data.String (IsString)

-- import Data.FileEmbed

helpText :: (IsString a) => a
helpText = "\
\GHCITUI MANUAL()                                            GHCITUI MANUAL()\n\
\\n\
\NAME\n\
\       GHCiTUI MANUAL -\n\
\\n\
\CLI SYNOPSIS\n\
\          Usage: ghcitui [OPTIONS] [TARGET]\n\
\\n\
\            ghcitui: A TUI interface for GHCi\n\
\\n\
\          Available options:\n\
\            -h,--help                Show this help text\n\
\            -d,--debug-console       Display the debug console\n\
\            -c,--cmd CMD             Command to start the internal\n\
\                                       interpreter\n\
\            -C,--workdir DIR         Set working dir\n\
\\n\
\STARTING AND STOPPING\n\
\   Starting\n\
\       GHCiTUI  runs a REPL in the current directory by default. By default,\n\
\       it launches cabal repl.\n\
\\n\
\          $ cd your/cabal/project/root/directory\n\
\          $ ghcitui\n\
\\n\
\       You can specify another starting directory with the -C <DIR> flag.\n\
\\n\
\          $ ghcitui -C some/other/directory\n\
\\n\
\   Stopping\n\
\       To quit, press <ESC> or q while in the code viewport panel  to  quit.\n\
\       While  not  in the code viewport panel, you may press <ESC> to get to\n\
\       the viewport panel.\n\
\\n\
\LAYOUT\n\
\       GHCiTUI is an in-terminal viewer for GHCi. The TUI is broken up  into\n\
\       three  primary panels, with some additional auxiliary panels for spe‐\n\
\       cial use cases:\n\
\\n\
\          ┌──────────────────┬──────┐\n\
\          │                  │ Info │\n\
\          │                  │      │\n\
\          │ Source Viewer    │      │\n\
\          │                  │      │\n\
\          │                  │      │\n\
\          ├──────────────────┤      │\n\
\          │                  │      │\n\
\          │ Live Interpreter │      │\n\
\          │                  │      │\n\
\          └──────────────────┴──────┘\n\
\\n\
\       Source Viewer: This panel shows source code. You can step,  continue,\n\
\       and toggle breakpoints among other operations in this panel.\n\
\\n\
\       Live Interpreter: This panel shows the GHCi/REPL passthrough. You can\n\
\       enter  expressions  and  GHCi  commands here like you would normally,\n\
\       with some additional keybindings.\n\
\\n\
\       Info: This panel displays miscellaneous info about whatever  is  cur‐\n\
\       rently  running.  For  example,  it can display the current bindings,\n\
\       loaded modules, and the current program trace.\n\
\\n\
\NAVIGATION\n\
\       At any point in time, you can revert back to the Source Viewer  panel\n\
\       with  the  <Esc> key, and you can always quit by hitting <Esc> in the\n\
\       Source Viewer panel.\n\
\\n\
\KEYBINDINGS\n\
\       At this time, keybindings are hardcoded. This will  hopefully  change\n\
\       in the future with a keybinding configuration file.\n\
\\n\
\   Source Viewer\n\
\       • Ctrl+x:  Toggle  between the Source Viewer and the Live Interpreter\n\
\         panels.\n\
\\n\
\       • <Esc>, q: Quit.\n\
\\n\
\       • <Up>, k: Move the cursor up. (j and k from Vim keybinds)\n\
\\n\
\       • <Down>, j: Move the cursor down. (j and k from Vim keybinds).\n\
\\n\
\       • <PgUp>: Move the source viewer one page up.\n\
\\n\
\       • <PgDown>: Move the source viewer one page down.\n\
\\n\
\       • +, -: Increase/decrease the Info panel size.\n\
\\n\
\       • b: Toggle breakpoint at current line. Not every line  in  a  source\n\
\         file can have a breakpoint placed on it.\n\
\\n\
\       • s: Advance execution by one step. Same as the :step in GHCi.\n\
\\n\
\       • c:  Advance  execution  until next breakpoint. Same as :continue in\n\
\         GHCi.\n\
\\n\
\       • t: Advance execution until next breakpoint under tracing.  Same  as\n\
\         :trace in GHCi.\n\
\\n\
\   Live Interpreter (REPL)\n\
\       • Ctrl+x:  Toggle  between the Source Viewer and the Live Interpreter\n\
\         panels.\n\
\\n\
\       • <Esc>: Switch to Source Viewer.\n\
\\n\
\       • <Up>: Scroll back in time through the REPL command history.\n\
\\n\
\       • <Down>: Scroll forward in time through the REPL command history.\n\
\\n\
\       • <PgUp>: Scroll the Live Interpreter window one page up.\n\
\\n\
\       • <PgDown>: Scroll the Live Interpreter window one page down.\n\
\\n\
\       • Ctrl+n: Toggle scrolling mode.\n\
\\n\
\       • +, - while in scrolling  mode:  Increase/decrease  the  live  panel\n\
\         size.\n\
\\n\
\       • <Enter>: Enter a command to the REPL.\n\
\\n\
\   Modules\n\
\       • Ctrl+x: Switch to the Live Interpreter.\n\
\\n\
\       • <Esc>: Switch to Source Viewer.\n\
\\n\
\       • <Up>, k: Move the module selection up.\n\
\\n\
\       • <Down>, j: Move the module selection down.\n\
\\n\
\       • +, -: Increase/decrease the info panel size.\n\
\\n\
\       • <Enter>, o: Open the selected module.\n\
\\n\
\                                                            GHCITUI MANUAL()"
