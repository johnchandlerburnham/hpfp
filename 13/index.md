# 13 Building Projects

Since I'm building these projects on NixOS, I have to modify how to build with
stack by putting the following `shell.nix` file in the project:

```
{ghc}:
with (import <nixpkgs> {});

haskell.lib.buildStackProject {
  inherit ghc;
  name = "myEnv";
  buildInputs = [ zlib ];
  buildPhase = ''
    export LANG=en_US.UTF-8
    '';
}
```

and then running stack with the `--nix` flag, or enabling nix in the
`stack.yaml` (or globally in `~/.stack/config.yaml`) with

```
nix:
  enable: true
```

## 13.6 More on importing modules

## Imtermission: Check your understanding

1. `forever`, `when`
2. `Data.Bits`, `Database.Blacktip.Types`
3. Types for `blacktip`'s database
4.
- a. `MV` is `Control.Concurrent.MVar`, `FPC` is `Filesystem.Path.CurrentOS`,
  `CC` is `Control.Concurrent`
- b. `Filesystem`
- c. `Control.Monad`

## Chapter Exercises

### Hangman game logic

See my [hangman project on GitHub](
https://github.com/johnchandlerburnham/hpfp/tree/master/13/hangman)

~~I debated whether or not to include code snippets of full-fledged stack
projects in this document. I've decided against it. It's one thing to include
self-contained modules as question answers, but if I were to, for example,
include the `Main.hs` file for the hangman project here, there'd be some
implicit dependencies like dict.txt and hangman.cabal, and if the past 524
pages of this book have taught me anything, implicit dependencies are bad
news.~~

Upon further reflection, I've decided to axe essentially all code snippets from
my notes. Code is much better in a type-checked source file than in a markdown
file where it can diverge from it's reference. I may add the snippets back if I
figure out a clever way to get `hakyll` to auto-include them. For now I'll just
add links to the files on Github.

### Modifying code

1. [See `CipherIO.hs`](/13/CipherIO.hs)
2. [See `ExitSuccess.hs`](/13/ExitSuccess.hs)
3. [See `ExitSuccess.hs`](/13/ExitSuccess.hs)
4. [See `Person.hs`](/13/Person.hs)

## 13.15 Follow-up resources

1. [Stack](https://github.com/commercialhaskell/stack)
2. [How I Start: Haskell](http://bitemyapp.com/posts/2014-11-18-how-i-start-haskell.html)
3. [Cabal FAQ](https://www.haskell.org/cabal/FAQ.html)
4. [Cabal user’s guide](https://www.haskell.org/cabal/users-guide/)
5. [A Gentle Introduction to Haskell, Modules chapter.](https://www.haskell.org/tutorial/modules.html)
