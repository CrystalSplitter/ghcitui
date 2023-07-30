#!/bin/sh

if ! which fswatch >/dev/null; then
    printf "%s\n" "'fswatch' is needed to run this script"
    exit 1
fi

fswatch -n 64 -ro -l1 -m poll_monitor app/ lib/ *.cabal | xargs -I{} cabal build
