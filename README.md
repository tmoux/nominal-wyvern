# nominal-wyvern

An implementation of the Nominal Wyvern type system written in Haskell

([wyvernlang.github.io](http://wyvernlang.github.io/))

## TODO
* ~~Refinements should overwrite old decls~~
* ~~Rework context lookups to return a ```Maybe Refinement```, then add functions to return a bool or return the value/exception as needed~~
* ~~Related to that, improve error messages (list the exprs/types, make lookup error messages specific)~~
* ~~Add syntax for annotated value (probably augment ```ValRef``` with a ```Maybe Type``` parameter) (this is to test things like the AB test)~~ AB test works (loops forever)
* ~~Find better way to handle bool operators/short circuiting in the monads~~
* ~~Go over rules, clean up/give better names to stuff~~
* ~~Implement graph generation rules~~
* Check for valid shape usage (probably at the same time, throw errors as needed)
* Check that nested type declarations work
* Look into the extra edges rules
* Later: REPL for typing/subtyping queries
