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
cabal run doctests -- arg1 arg2
```

**At the moment, using `cabal test` is not reliable. See [#22](https://github.com/martijnbastiaan/doctest-parallel/issues/22).**

## Stack
Stack users can use:

```
stack test
```
