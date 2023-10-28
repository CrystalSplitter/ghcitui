======
MANUAL
======

------------
INTRODUCTION
------------

GHCiTUI is an in-terminal viewer for GHCi. The TUI is broken up into three
primary panels, with some additional auxiliary panels for special use cases:

::

  ┌──────────────────┬──────┐
  │                  │ Info │
  │                  │      │
  │ Source Viewer    │      │
  │                  │      │
  │                  │      │
  ├──────────────────┤      │
  │                  │      │
  │ Live Interpreter │      │
  │                  │      │
  └──────────────────┴──────┘

**Source Viewer:** This panel shows source code. You can step, continue,
and toggle breakpoints among other operations in this panel.

**Live Interpreter:** This panel shows the GHCi/REPL passthrough. You can
enter expressions and GHCi commands here like you would normally.

**Info:** This panel displays miscellaneous info about whatever is
currently running. For example, it can display the current bindings and loaded
modules.

----------
Navigation
----------

------------------
Keybinds Reference
------------------

*************
Source Viewer
*************

- ``Ctrl+x``: to toggle between the Source Viewer and the Live Interpreter
  panels.
- ``<Esc>``, ``q``: Quit.
- ``Up``, ``k``: Move the selected line up. (``j`` and ``k`` from Vim keybinds)
- ``Down``, ``j``: Move the selected line down. (``j`` and ``k`` from Vim
  keybinds).
- ``PgUp``: Move the source viewer one page up.
- ``PgDown``: Move the source viewer one page down.
- ``b``: Toggle breakpoint at current line. Not every line in a source file can
  have a breakpoint placed on it.
- ``s``: Advance execution by one step. Same as the ``:step`` in GHCi.
- ``c``: Advance execution until next breakpoint. Same as ``:continue`` in
  GHCi.

