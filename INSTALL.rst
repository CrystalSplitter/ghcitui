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
   #. Installing

======================
Working with the source
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
we build.

.. code:: shell

  # User install (installs to $HOME/.cabal)
  cabal configure -O --installdir="$HOME/.cabal" -j

  # Global install (will require administrator privileges to actually install)
  cabal configure -O -j

For contributors, you'll want to enable some useful testing flags, so run this
after the above:

.. code:: shell

  cabal configure \
    -O \
    --test-option='--color' \
    --test-show-details=streaming

Now we can build the GHCiTUI executable

.. code:: shell

  cabal build

-------
Testing
-------

Unit tests are run automatically during installation, but you can also run them
manually with:

.. cabal:: shell

  cabal test

----------
Installing
----------

To install:

.. code:: shell

  # Per-user install (recommended)
  cabal install

  # Install globally (note this may require root privileges)
  cabal install --install-method=copy --installdir=/usr/local/bin
