# nominal-wyvern

An implementation of the Nominal Wyvern type system written in Haskell

([wyvernlang.github.io](http://wyvernlang.github.io/))

## Installing
Installation uses [cabal](https://www.haskell.org/cabal/).
Please run:
```
cabal configure
cabal build
cabal install
```

Then you should be able to run:
```
nominal-wyvern [file]
```
Please see the ```examples/``` folder for some example programs.

## TODO
* Check that nested type declarations work
* Check for valid shape usage (probably at the same time, throw errors as needed)
* Look into the extra edges rules
* Later: REPL for typing/subtyping queries
