======================
BUILD AND INSTALLATION
======================

This document describes how to build, install, and test the GHCiTUI program.

GHCiTUI is configured, built, and installed through `Cabal`_. Cabal can be
installed through the ``ghcup`` command line tool. Familiarity with Cabal
is not necessary to build GHCiTUI.

------------------
Getting the source
------------------

Currently the source code is hosted on https://github.com. You can check out
the source code using ``git`` with SSH:

.. code:: shell

  # SSH (for contributors, requires SSH set up)
  git clone git@github.com:CrystalSplitter/ghcitui.git

Or HTTPS:

.. code:: shell

  # HTTPS
  git clone https://github.com/CrystalSplitter/ghcitui.git

--------
Building
--------

The GHCiTUI cabal configurations defaults to release builds in the
`cabal.project`_. Therefore, we can just run...

.. code:: shell

  cabal build  # Build library and executable (release mode)
  cabal test  # Run tests (optional)
  cabal install  # Per-user install (Nix-Style)

By default, this will place the ``ghcitui`` executable in
``$HOME/.cabal/bin``.

Contributors may find it useful to make their own
``cabal.project.local`` with ``cabal configure`` while debugging.

.. _Cabal: https://www.haskell.org/cabal/
.. _cabal.project: ./cabal.project
