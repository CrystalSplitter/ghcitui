**********************
BUILD AND INSTALLATION
**********************

This document describes how to build, install, and test the GHCiTUI program.

At present, installation has only been tested on Linux systems. While
installation may work on other operating systems, you may encounter issues.

GHCiTUI is configured, built, and installed through `Cabal`_. Cabal can be
installed through the ``ghcup`` command line tool. Familiarity with Cabal
is not necessary to build GHCiTUI.

Installing Latest Stable From Hackage
=====================================

The simplest way to install GHCiTUI is by downloading the package from Hackage:

.. code-block:: shell

  cabal update && cabal install ghcitui
  # And to check it was successfully installed...
  ghcitui --version
  # ghcitui <VERSION>


If this fails, see `Troubleshooting`_ below for possible
fixes.

Getting the source
==================

Currently the source code is hosted at
https://github.com/CrystalSplitter/ghcitui. You can check out the source code
using ``git``.

Note however that the repository is configured with ``git lfs``
to store documentation images, so if you want images in your checkout, you
must `install git-lfs separately`_.

Checking out with SSH:

.. code-block:: shell

  # SSH (for contributors, requires SSH set up)
  git clone git@github.com:CrystalSplitter/ghcitui.git

Or HTTPS:

.. code-block:: shell

  # HTTPS
  git clone https://github.com/CrystalSplitter/ghcitui.git

Building
========

The GHCiTUI cabal configurations defaults to release builds in the
`cabal.project`_. Therefore, we can just run...

.. code-block:: shell

  cd ghcitui
  cabal build all  # Build library and executable (release mode)
  cabal test all  # Run tests (optional)
  cabal install exe:ghcitui  # Per-user install (Nix-Style)

By default, this will place the ``ghcitui`` executable in
``$HOME/.cabal/bin``.

Contributors may find it useful to make their own
``cabal.project.local`` with ``cabal configure`` while debugging.


Troubleshooting
===============

Here we list some known build or install issues.

Cannot find -ltinfo
-------------------

If you see this error (or similar) when installing ``ghcitui``:

.. code-block::

    -->8--
    /usr/bin/ld.gold: error: cannot find -ltinfo
    collect2: error: ld returned 1 exit status
    `gcc' failed in phase `Linker'. (Exit code: 1)
    Error: cabal: Failed to build vty-unix-0.2.0.0
    -->8--

This usually means there's a missing linking dependency on libtinfo which can
be satisfied by an ncurses development package. On Debian-based systems, this
can be installed with:

.. code-block:: shell
    
    sudo apt install libncurses-dev

.. _Cabal: https://www.haskell.org/cabal/
.. _cabal.project: ./cabal.project
.. _install git-lfs separately: https://git-lfs.com/

