# Deep Into the Code: Haskell

My Haskell notes comprise of several source code files. I've written them this way to verify the correctness of the syntax of my notes.

`haskell.hs` may be run or compiled from the terminal while the numbered files are intended to be loaded from within the GHCi REPL environment.

My current preferred way to install Haskell on macOS is by using [Homebrew](https://brew.sh) to install [The Haskell Tool Stack](https://haskellstack.org/). If you're on a Mac, go to your terminal and run the following installation command.

```shell
brew install haskell-stack
```

After the installation completes, make sure you're within the directory of your local copy of my Haskell notes, and the run the command

```shell
stack runghc haskell
```

to compile-on-the-fly and execute the `haskell.hs` file. If successful, it should result in the simple output of `Hello world!`.

The other files are not meant to be run directly. Instead, they're intended to be loaded from within the GHCi REPL environment. While remaining in the same directory, open up GHCi by running:

```shell
stack ghci
```

Then use GHCi's load command, `:l` (short for `:load`), to load a file into the REPL:

```shell
:l 01-comments
```

The preceding example loads the `01-comments.hs` file. If there were any compile-time errors, GHCi will show them.

I would need to include a `main` function in every file to make them directly runnable but I didn't want to add that extra "noise" to them.

Perhaps I'll find a better way when I get better at Haskell.
