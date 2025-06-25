# Revision history for ghcitui

## 0.4.1.1 -- 2025-06-25

### Misc

- Updated dependency ranges. No new features or bug fixes.

## 0.4.1.0 -- 2024-11-16

### Bug fixes

- Fixed dependency error with `fsnotify` failing to build due to
  `text` show conflict.

## 0.4.0.0 -- 2024-11-15

### New features

- Interruptable expressions! This was a huge rework of the code. You can now press `Ctrl+c`
  and expression evaluation will be interrupted! Very handy for avoiding hanging.
  See [GitHub Issue #49](https://github.com/CrystalSplitter/ghcitui/issues/49) for details.

### API changes

Large overhaul in general, as GHCiTUI has moved to an async daemon scheduling model.

- **Ghcitui.Brick**
  - Introduction of the new `CustomAppEvent` to handle new event handling.
  - The `brickApp` now specifies a `CustomAppEvent`.
  - Moved event utils to `EventUtils.hs`
  - Separated out `Events.hs` into `InterpWindowEvents.hs` and `SourceWindowEvents.hs`.
  - Introduced the callback functions `handleSourceWindowPostCb` and `interpWindowPostCb`.
- **Ghcitui.Core**
  - Removed the `run` command as it was misleading in an asynchronous context. Replaced with
    `threadUnsafeRun`.
  - Added the `schedule` and `scheduleWithCb` functions, which mostly replace the intent
    of `run`, but work with async.
  - Added `interruptDaemon` to call the interrupt signal.
  - `emptyInterpreterState` now must be run under `IO`, as it must set up the lock.
  - Added `readyToExec` to check if the `ghci` handle lock is taken.

In general, lots of doc fixes.

### Bug fixes

- Fixed a bug where the module display wouldn't reveal the source in the `Source Window`.
  when there was only one module. See [GitHub Issue #48](https://github.com/CrystalSplitter/ghcitui/issues/48)
  for details.

### Known issues

See https://github.com/CrystalSplitter/ghcitui/issues for the latest issues.

- Inability to suspend operation through `Ctrl+z`.

## 0.3.0.0 -- 2024-03-17

### API changes

- **Ghcitui.Brick**
  - Large rework of SourceWindow's end calculation.
    - Removed `updateSrcWindowEnd`, replaced with `updateVerticalSpace`.
    - Added `srcWindowLineDiffCount`.

### Bug fixes

- Can now parse functions with apostraphes in names. (Issue #38)
- Switching between files when updating contexts now snaps to the stopped line (Issue #41)

### Known issues

See https://github.com/CrystalSplitter/ghcitui/issues for the latest issues.

- Unable to interrupt expressions (fixed in 0.4.0.0)
- Inability to suspend operation through `Ctrl+z`.

## 0.2.0.0 -- 2024-02-11

### New features

- Rudimentary tab completion! (credit: https://github.com/bradrn)

### API changes

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
- Unable to interrupt expressions (fixed in 0.4.0.0)
- Inability to suspend operation through `Ctrl+z`.

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
- Unable to interrupt expressions (fixed in 0.4.0.0)
- Currently no remapping of keybindings or colours.
- CRLF line endings don't work (fixed in 0.2.0.0)
- Inability to suspend operation through `Ctrl+z`.
