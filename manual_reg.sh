#!/bin/sh
# Simple script for manual testing
cabal install
for file in examples/*.wyv; do
    timeout 1s nominal-wyvern $file
    echo Press Enter to proceed...
    read input
done
