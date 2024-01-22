# Revision history for ghcitui

## 0.1.0.0 -- 2024-01-21

First release! This is a "public beta" release, which we try to get feedback for higher priority
features.

### Features

- The public Ghcitui library.
- Ghcid connection set up.
- Source code viewer.
- GHCi REPL
- Current Bindings.
- Available Modules.
- Tracing.
- Debug console.

### Bug fixes

- None--this is the first release.

### Known issues

(See https://github.com/CrystalSplitter/ghcitui/issues for the latest issues.)

- Occasionally we get a SEGV on start up. Uncertain why. Very infrequent--likely a race condition
  in Vty or GHCiD?
- String variables which contain quotes are not parsed correctly.
- Unable to interrupt expressions (hopefully fixed in a future version?)
- Currently no remapping of keybindings or colours.
