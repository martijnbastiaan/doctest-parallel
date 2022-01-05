# Integrating `doctest-parallel` into your project
`doctest-parallel` currently assumes you have a `.cabal` file in your project. If this is the case, it can be setup by adding the following:

## `your-project.cabal`
```
test-suite doctests
  type:             exitcode-stdio-1.0
  hs-source-dirs:   test
  main-is:          doctests.hs
  ghc-options:      -threaded
  build-depends:    base, your-project, doctest-parallel >= 0.1
  default-language: Haskell2010
```

## `cabal.project`
You can omit this file if your project is a Stack-only project. Otherwise, add the following:

```
write-ghc-environment-files: always
```

If this file does not yet exist, also add:

```
packages:
  .
```

## `test/doctests.hs`
```haskell
module Main where

import Test.DocTest (mainFromCabal)
import System.Environment (getArgs)

main :: IO ()
main = mainFromCabal "your-project" =<< getArgs
```

# Running the testsuite

## Cabal
Execute:

```
cabal run doctests
```

**At the moment, using `cabal test` is not reliable. See [#22](https://github.com/martijnbastiaan/doctest-parallel/issues/22).**

## Stack
Stack users can use:

```
stack test example:doctestsstack test
```

It will also run as part of `stack test`.

# Help
Run:

```
cabal run doctests -- --help
```

Or:

```
stack test example:doctests --test-arguments --help
```

Example output:

```
Usage:
  doctest [ options ]... [<module>]...
  doctest --help
  doctest --version
  doctest --info

Options:
   -jN                      number of threads to use
†  --randomize-order        randomize order in which tests are run
†  --seed=N                 use a specific seed to randomize test order
†  --preserve-it            preserve the `it` variable between examples
   --verbose                print each test as it is run
   --quiet                  only print errors
   --help                   display this help and exit
   --version                output version information and exit
   --info                   output machine-readable version information and exit

Supported inverted options:
†  --no-randomize-order (default)
†  --no-preserve-it (default)

Options marked with a dagger (†) can also be used to set module level options, using
an ANN pragma like this:

  {-# ANN module "doctest-parallel: --no-randomize-order" #-}

```
