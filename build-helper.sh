#!/bin/bash

DARCS284=darcs-2.8.4
DARCS285=darcs-2.8.5

set -e

if [[ ! -d "$DARCS284" && ! -d "$DARCS285" ]]; then
    echo "Downloading darcs 2.8.4 to $DARCS284"
    wget -q -O - http://hackage.haskell.org/package/darcs-2.8.4/darcs-2.8.4.tar.gz | tar xvzf -
fi

if [[ ! -d "$DARCS285" ]]; then
    echo "Generating darcs 2.8.5"
    cp -R $DARCS284 $DARCS285
    patch "$DARCS285/darcs.cabal" < darcs2.8.5.patch
fi
