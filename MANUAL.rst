==============
GHCiTUI MANUAL
==============

------------
CLI Synopsis
------------

.. code-block::

  Usage: ghcitui [--version] [--debug-console] [-v] [--daemon-log LOGFILE]
                [-c|--cmd CMD] [-C|--workdir DIR] [TARGET]

    ghcitui: A TUI interface for GHCi

  Available options:
    -h,--help                Show this help text
    --version                Print the version number and exit
    --debug-console          Display the debug console
    -v                       Set verbosity for output logs. Pass multiple times
                            (e.g -vvv) to increase the logging. Use --daemon-log
                            to specify where the logs go.
    --daemon-log LOGFILE     File path for debugging daemon logs. Used with -v.
                            Setting this to 'stdout' or 'stderr' sends logs to
                            each, respectively. Defaults to /tmp/ghcitui.log.
    -c,--cmd CMD             Command to start the internal interpreter
    -C,--workdir DIR         Set working dir

---------------------
Starting and Stopping
---------------------

********
Starting
********

GHCiTUI runs a REPL in the current directory by default. By default, it
launches ``cabal repl``.

.. code-block:: bash

  $ cd your/cabal/project/root/directory
  $ ghcitui

You can specify another starting directory with the ``-C <DIR>`` flag.


.. code-block:: bash

  $ ghcitui -C some/other/directory


********
Stopping
********

To quit, press ``<ESC>`` or ``q`` while in the code viewport panel to quit.
While not in the code viewport panel, you may press ``<ESC>`` to get to the
viewport panel.

------
Layout
------

GHCiTUI is an in-terminal viewer for GHCi. The TUI is broken up into three
primary panels, with some additional auxiliary panels for special use cases:

.. code-block::

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
enter expressions and GHCi commands here like you would normally, with some
additional keybindings.

**Info:** This panel displays miscellaneous info about whatever is
currently running. For example, it can display the current bindings, loaded
modules, and the current program trace.

----------
Navigation
----------

At any point in time, you can revert back to the Source Viewer panel with the
``<Esc>`` key, and you can always quit by hitting ``<Esc>`` in the Source Viewer
panel.

On top of each panel is a label where the navigation key combination is located.
For example, the key combination above the Modules panel displays ``[M]``.
Pressing this key combination will move the focus to that panel.

-----------
Keybindings
-----------

At this time, keybindings are hardcoded. This will hopefully change in the
future with a keybinding configuration file.

*************
Source Viewer
*************

- ``?``: Display help inside GHCiTUI.
- ``Ctrl+x``: Toggle between the Source Viewer and the Live Interpreter
  panels.
- ``M``: Switch to the module panel.
- ``<Esc>``, ``q``: Quit.
- ``<Up>``, ``k``: Move the cursor up. (``j`` and ``k`` from Vim keybinds)
- ``<Down>``, ``j``: Move the cursor down. (``j`` and ``k`` from Vim keybinds).
- ``<PgUp>``: Move the source viewer one page up.
- ``<PgDown>``: Move the source viewer one page down.
- ``+``, ``-``: Increase/decrease the left panel sizes.
- ``b``: Toggle breakpoint at current line. Not every line in a source file can
  have a breakpoint placed on it.
- ``s``: Advance execution by one step. Same as the ``:step`` in GHCi.
- ``c``: Advance execution until next breakpoint. Same as ``:continue`` in
  GHCi.
- ``t``: Advance execution until next breakpoint under tracing. Same as
  ``:trace`` in GHCi.

***********************
Live Interpreter (REPL)
***********************

- ``Ctrl+x``: Toggle between the Source Viewer and the Live Interpreter
  panels.
- ``<Esc>``: Switch to Source Viewer.
- ``<Esc>`` while in scrolling mode: Exit scrolling mode.
- ``<Up>``: Scroll back in time through the REPL command history.
- ``<Down>``: Scroll forward in time through the REPL command history.
- ``<PgUp>``: Scroll the Live Interpreter window one page up.
- ``<PgDown>``: Scroll the Live Interpreter window one page down.
- ``Ctrl+n``: Toggle scrolling mode.
- ``+``, ``-`` while in scrolling mode: Increase/decrease the live
  panel size.
- ``<Enter>``: Enter a command to the REPL.

*******
Modules
*******

- ``?``: Display help inside GHCiTUI.
- ``Ctrl+x``: Switch to the Live Interpreter.
- ``<Esc>``, ``C``: Switch to Source Viewer.
- ``<Up>``, ``k``: Move the module selection up.
- ``<Down>``, ``j``: Move the module selection down.
- ``+``, ``-``: Increase/decrease the info panel size.
- ``<Enter>``, ``o``: Open the selected module.
