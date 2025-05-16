# minicheck

**minicheck** is a CTL (Computation Tree Logic) model checker written in Haskell.

## 🔧 Building

Make sure you have [GHC](https://www.haskell.org/ghcup/) and [cabal](https://www.haskell.org/cabal/) installed.

Clone the project and run:

```bash
cabal build
```

To get the path to the built executable:

```bash
cabal list-bin minicheck
```

Optionally, define a shell alias for convenience:

```bash
alias minicheck=$(cabal list-bin minicheck)
```

Now you can run it like this:
```bash
minicheck --ts=data/soda.txt
```

## 🏃 Running

Basic usage:
```bash
minicheck --ts=<file> [--help]
```

* `--ts=<file>`: Path to the transition system file
* `--help`: Show usage information and exit
