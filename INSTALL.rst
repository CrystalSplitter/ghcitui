======================
BUILD AND INSTALLATION
======================

This document describes how to build, install, and test the GHCiTUI program.

-----------------
Table of contents
-----------------

#. Working with the source

   #. Getting the source
   #. Building
   #. Testing

#. Installing from source

======================
Installing from source
======================

GHCiTUI is configured, built, and installed through ``cabal``:
https://www.haskell.org/cabal/. Cabal can be installed through the ``ghcup``
command line tool. Familiarity with Cabal is not necessary to build GHCiTUI.

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

We should set up the Cabal project file with some configuration options before
we build:

.. code:: shell

  cabal configure -O 

For contributors, you'll want to enable testing:

.. code:: shell

  cabal configure -O \
    --enable-tests \
    --test-option='--color' \
    --test-show-details=streaming
