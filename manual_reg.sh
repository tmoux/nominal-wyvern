#!/bin/sh
# Simple script for manual testing
ghc Main.hs -o Main
for file in examples/*.wyv; do
    timeout 1s ./Main $file
    echo Press Enter to proceed...
    read input
done
