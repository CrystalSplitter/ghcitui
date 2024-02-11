# GHCiTUI: Interactive terminal interface for the Glasgow Haskell Compiler

```
          /       ______    __  __    ______    __
        //       /\  ___\  /\ \_\ \  /\  ___\  /\_\
       //     ___\ \ \__ \_\ \  __ \_\ \ \_____\ \ \___
' , _ //      \   \ \_____\ \ \_\ \_\ \ \_____\ \ \_\  \
 / \ // 7      \   \/_____/  \/_/\/_/  \/_____/  \/_/   \
    "   \       \           ______   __  __    __        \
    a   a        \         /\__  _\ /\ \/\ \  /\ \        \
 |_      \        \________\/_/\ \/_\ \ \_\ \_\ \ \________\
   '._    '                   \ \_\  \ \_____\ \ \_\
     (' _ '                    \/_/   \/_____/  \/_/
```

![GitHub Actions Workflow Status](https://img.shields.io/github/actions/workflow/status/CrystalSplitter/ghcitui/haskell.yaml) ![Hackage Version](https://img.shields.io/hackage/v/ghcitui)

This is an experimental front-end terminal interface for
`ghci`. It provides a source viewer, keybindings, an interactive
interpreter, and a local context viewer.

![Splash Image For GHCiTUI](https://media.githubusercontent.com/media/CrystalSplitter/ghcitui/main/docs/assets/20240116_splash.png)

## Installation

You can install this project from Hackage using `cabal` or from source. See [INSTALLATION] for details.

## Basic Usage

For full usage, please see the [manual].

### Starting the TUI

GHCiTUI runs a repl in the current directory by default.

```bash
$ cd your/cabal/project/root/directory
$ ghcitui
```

You can specify another directory with the `-C <DIR>` flag.

```bash
$ ghcitui -C some/other/directory
```

### Quitting the TUI

Press `<ESC>` or `q` while in the code viewport panel to quit. While not in the
code viewport panel, you may press `<ESC>` to get to the viewport panel.

## Contributing

Contributors are welcome! Please see [CONTRIBUTING] to see how.

[INSTALLATION]: https://github.com/CrystalSplitter/ghcitui/blob/main/INSTALL.rst
[manual]: https://github.com/CrystalSplitter/ghcitui/blob/main/MANUAL.rst
[CONTRIBUTING]: https://github.com/CrystalSplitter/ghcitui/blob/main/CONTRIBUTING.md
