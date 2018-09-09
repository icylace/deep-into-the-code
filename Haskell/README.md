# Deep Into the Code: Haskell

My Haskell notes comprise of several source code files. I've written them this way to verify the correctness of the syntax of my notes.

For instance, `haskell.hs` may be compiled and run from the command line or loaded and run from within the GHCi REPL environment.

My current preferred way to install Haskell on macOS is by using [Homebrew](https://brew.sh) to install [The Haskell Tool Stack](https://haskellstack.org/). If you're on a Mac, go to your terminal and run the following installation command.

```shell
brew install haskell-stack
```

After the installation completes, make sure you're within the directory of your local copy of my Haskell notes, and the run the command

```shell
stack runghc haskell
```

to compile-on-the-fly and execute the `haskell.hs` file. If successful, it should result in the simple output of `Hello world!`. Otherwise, compiler errors will be shown.

Another way is to load the source code from within the GHCi REPL environment. While remaining in the same directory, open up GHCi by running

```shell
stack ghci
```

then use GHCi's load command, `:l` (short for `:load`), to load a file into the REPL:

```shell
:l haskell
```

The preceding example loads the `haskell.hs` file. If there were any compile-time errors, GHCi will show them.

## Resources

### Tools

- [The Glasgow Haskell Compiler](https://www.haskell.org/ghc/)

#### Package Collections

- [Hackage](https://hackage.haskell.org/)
- [Stackage](https://www.stackage.org/)

#### IDE Integration

- [intero](https://github.com/commercialhaskell/intero)
- [Haskero](https://marketplace.visualstudio.com/items?itemName=Vans.haskero)

### Learning

- [Haskell Programming from First Principles](http://haskellbook.com/)
- [What I Wish I Knew When Learning Haskell](http://dev.stephendiehl.com/hask/)
- [Structuring your first Haskell project with Stack](http://sakshamsharma.com/2018/03/haskell-proj-struct/index.html)

### References

- [Haskell 2010 Language Report](https://www.haskell.org/onlinereport/haskell2010/)
- [Haskell](https://www.haskell.org/)
- [Haskell (alternative)](https://haskell-lang.org/)
- [Haskell (alternative) - Operator Glossary](https://haskell-lang.org/tutorial/operators)
- [The Haskell Programming Language wiki](https://wiki.haskell.org/Haskell)
- [The Haskell Programming Language wiki - Keywords](https://wiki.haskell.org/Keywords)
- [A Guide to GHC's Extensions](https://limperg.de/ghc-extensions/)
- [Haskell/GHC symbol search cheatsheet](https://github.com/takenobu-hs/haskell-symbol-search-cheatsheet)
