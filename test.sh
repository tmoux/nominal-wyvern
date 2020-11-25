#!/bin/sh
# Simple script for manual testing
cabal build
for file in examples/*.wyv; do
    echo Testing $file...
    timeout 1s cabal run nominal-wyvern $file
    echo Press Enter to proceed...
    read input
done
