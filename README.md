# nominal-wyvern

An implementation of the Nominal Wyvern type system written in Haskell

([wyvernlang.github.io](http://wyvernlang.github.io/))

## Installing
Installation uses [cabal](https://www.haskell.org/cabal/).
Ensure you have GHC 8.8.4 (you can use [ghcup](https://www.haskell.org/ghcup/)).
Please run
```
cabal configure
cabal build
```

Then you should be able to run
```
cabal test --test-show-details=direct
cabal run nominal-wyvern [file]
```
Please see the ```examples/``` folder for some example programs.
