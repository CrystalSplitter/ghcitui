#!/bin/sh
fswatch -n 64 -ro -l1 -m poll_monitor app/ lib/ | xargs -I{} cabal build
