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

This is an experimental front-end terminal interface for 
`ghci`. It provides a source viewer, keybindings, an interactive
interpreter, and a local context viewer.

## Installation

As this project is experimental, currently installing from
source is the only option.

See [INSTALLATION.rst](./INSTALL.rst) for details.

## Basic Usage

For detailed usage, please see the [manual](./MANUAL.rst).

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

Contributors are welcome! Please see [CONTRIBUTING.md](./CONTRIBUTING.md)
to see how.
