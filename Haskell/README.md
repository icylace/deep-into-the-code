# Deep Into the Code: Haskell (A WORK-IN-PROGRESS)

My Haskell notes are comprised of several source code files. I've written them this way to verify the correctness of the syntax of my notes.

For instance, `GeneralLanguage/src/Main.hs` may be compiled and run from the command line or loaded and run from within the GHCi REPL environment.

I use macOS and my current preferred way to install Haskell is by using [Nix](https://nixos.org/nix/) to install [The Haskell Tool Stack](https://haskellstack.org/).

If you're on a Mac and you don't already have Nix, go to your terminal and run this installation command:

```shell
curl https://nixos.org/nix/install | sh
```

Now if you don't already have Stack installed, run this installation command:

```shell
nix-env --install --attr nixpkgs.stack
```

After the installation completes, make sure you're within the directory of your local copy of my Haskell notes, and the run the command

```shell
stack runghc haskell
```

to compile-on-the-fly and execute the `GeneralLanguage/src/Main.hs` file. If successful, it should result in the simple output of `Hello world!`. Otherwise, compiler errors will be shown.

Another way is to load the source code from within the GHCi REPL environment. While remaining in the same directory, open up GHCi by running

```shell
stack ghci
```

then use GHCi's load command, `:l` (short for `:load`), to load a file into the REPL:

```shell
:l haskell
```

The preceding example loads the `GeneralLanguage/src/Main.hs` file. If there were any compile-time errors, GHCi will show them.

## Other Details

- [Haskell Resources](./Resources.md)
- [Haskell Symbols](./Symbols.md)
- [Haskell Style](./Style.md)
