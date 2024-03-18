# Revision history for ghcitui

## 0.3.0.0 -- 2024-03-17

### API Changes

- **Ghcitui.Brick**
  - Large rework of SourceWindow's end calculation.
    - Removed `updateSrcWindowEnd`, replaced with `updateVerticalSpace`.
    - Added `srcWindowLineDiffCount`.

### Bug fixes

- Can now parse functions with apostraphes in names. (Issue #38)
- Switching between files when updating contexts now snaps to the stopped line (Issue #41)

### Known issues

See https://github.com/CrystalSplitter/ghcitui/issues for the latest issues.

## 0.2.0.0 -- 2024-02-11

### New Features

- Rudimentary tab completion! (credit: https://github.com/bradrn)

### API Changes

- **Ghcitui.Brick**
  - Added functions to support tab completion.
- **Ghcitui.Core**
  - Added `Ghcitui.Ghcid.ParseTabCompletions` module.
  - Added `Ghcitui.Ghcid.Daemon.tabComplete`.
  - Moved ParseError to its own module (`Ghcitui.Ghcid.ParseError`).
  - Removed lazy data fields in records in
    - `Ghcitui.Ghcid.Daemon.BreakpointArg`
    - `Ghcitui.Ghcid.Daemon.InterpState`
    - Possibly a few more
- **Other**
  - Update version for `vty`.

### Bug fixes

- Fix issue with CRLF line endings caused weird source viewer wrapping behaviour.

### Misc

- Added help message on start up splash to mention '?' keybinding.
- Increased the cabal `tested-with` range.

### Known issues

See https://github.com/CrystalSplitter/ghcitui/issues for the latest issues.

- Can't parse functions with apostrophes in names. (Issue #38) (fixed in 0.3.0.0)
- Switching between files when updating contexts does not snap to the stopped line (Issue #41)
  (fixed in 0.3.0.0)

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
- CRLF line endings don't work (fixed in 0.2.0.0)
