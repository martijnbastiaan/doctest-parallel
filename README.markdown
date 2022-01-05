
# Doctest parallel: Test interactive Haskell examples

`doctest-parallel` is a library that checks [examples in Haddock comments](http://www.haskell.org/haddock/doc/html/ch03s08.html#id566093).  It is similar to the [popular Python module with the same name](http://docs.python.org/library/doctest.html).

# Installation
`doctest-parallel` is available from [Hackage](https://hackage.haskell.org/package/doctest-parallel). It cannot be used as a standalone binary, rather, it expects to be integrated in a Cabal/Stack project. See [examples/](example/README.md) for more information on how to integrate `doctest-parallel` into your project.

# Migrating from `doctest`
See [issue #11](https://github.com/martijnbastiaan/doctest-parallel/issues/11) for more information.

# Usage
Below is a small Haskell module. The module contains a Haddock comment with some examples of interaction. The examples demonstrate how the module is supposed to be used.

```haskell
module Fib where

-- | Compute Fibonacci numbers
--
-- Examples:
--
-- >>> fib 10
-- 55
--
-- >>> fib 5
-- 5
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
```

A comment line starting with `>>>` denotes an _expression_. All comment lines following an expression denote the _result_ of that expression. Result is defined by what a [REPL](http://en.wikipedia.org/wiki/Read-eval-print_loop) (e.g. ghci) prints to `stdout` and `stderr` when evaluating that expression.

`doctest-parallel` will fail on comments that `haddock` also doesn't like. Sometimes (e.g., [#251](https://github.com/sol/doctest/issues/251)), this means that `doctest-parallel` will fail on input that GHC accepts.

## Example groups

Examples from a single Haddock comment are grouped together and share the same
scope.  E.g. the following works:

```haskell
-- |
-- >>> let x = 23
-- >>> x + 42
-- 65
```

If an example fails, subsequent examples from the same group are skipped.  E.g.
for

```haskell
-- |
-- >>> let x = 23
-- >>> let n = x + y
-- >>> print n
```

`print n` is not tried, because `let n = x + y` fails (`y` is not in scope!).

## Setup code

You can put setup code in a [named chunk][named-chunks] with the name `$setup`.
The setup code is run before each example group.  If the setup code produces
any errors/failures, all tests from that module are skipped.

Here is an example:

```haskell
module Foo where

import Bar.Baz

-- $setup
-- >>> let x = 23 :: Int

-- |
-- >>> foo + x
-- 65
foo :: Int
foo = 42
```

Note that you should not place setup code in between the module header (`module
...  where`) and import declarations. GHC will not be able to parse it ([issue
 #167](https://github.com/sol/doctest/issues/167)). It is best to place setup
code right after import declarations, but due to its declarative nature you can
place it anywhere in between top level declarations as well.


## Multi-line input
GHCi supports commands which span multiple lines, and the same syntax works for doctest:

```haskell
-- |
-- >>> :{
--  let
--    x = 1
--    y = 2
--  in x + y + multiline
-- :}
-- 6
multiline = 3
```

Note that `>>>` can be left off for the lines following the first: this is so that
haddock does not strip leading whitespace. The expected output has whitespace
stripped relative to the :}.

Some peculiarities on the ghci side mean that whitespace at the very start is lost.
This breaks the example `broken`, since the x and y aren't aligned from ghci's
perspective.  A workaround is to avoid leading space, or add a newline such
that the indentation does not matter:

```haskell
{- | >>> :{
let x = 1
    y = 2
  in x + y + works
:}
6
-}
works = 3

{- | >>> :{
 let x = 1
     y = 2
  in x + y + broken
:}
3
-}
broken = 3
```

## Multi-line output
If there are no blank lines in the output, multiple lines are handled
automatically.

```haskell
-- | >>> putStr "Hello\nWorld!"
-- Hello
-- World!
```

If however the output contains blank lines, they must be noted
explicitly with `<BLANKLINE>`. For example,

```haskell
import Data.List ( intercalate )

-- | Double-space a paragraph.
--
--   Examples:
--
--   >>> let s1 = "\"Every one of whom?\""
--   >>> let s2 = "\"Every one of whom do you think?\""
--   >>> let s3 = "\"I haven't any idea.\""
--   >>> let paragraph = unlines [s1,s2,s3]
--   >>> putStrLn $ doubleSpace paragraph
--   "Every one of whom?"
--   <BLANKLINE>
--   "Every one of whom do you think?"
--   <BLANKLINE>
--   "I haven't any idea."
--
doubleSpace :: String -> String
doubleSpace = (intercalate "\n\n") . lines
```

## Matching arbitrary output
Any lines containing only three dots (`...`) will match one or more lines with
arbitrary content. For instance,

```haskell
-- |
-- >>> putStrLn "foo\nbar\nbaz"
-- foo
-- ...
-- baz
```

If a line contains three dots and additional content, the three dots will match
anything *within that line*:

```haskell
-- |
-- >>> putStrLn "foo bar baz"
-- foo ... baz
```

## QuickCheck properties

Haddock (since version 2.13.0) has markup support for properties.  Doctest can
verify properties with QuickCheck.  A simple property looks like this:

```haskell
-- |
-- prop> \xs -> sort xs == (sort . sort) (xs :: [Int])
```

The lambda abstraction is optional and can be omitted:

```haskell
-- |
-- prop> sort xs == (sort . sort) (xs :: [Int])
```

A complete example that uses setup code is below:

```haskell
module Fib where

-- $setup
-- >>> import Control.Applicative
-- >>> import Test.QuickCheck
-- >>> newtype Small = Small Int deriving Show
-- >>> instance Arbitrary Small where arbitrary = Small . (`mod` 10) <$> arbitrary

-- | Compute Fibonacci numbers
--
-- The following property holds:
--
-- prop> \(Small n) -> fib n == fib (n + 2) - fib (n + 1)
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
```

If you see an error like the following, ensure that [QuickCheck](http://hackage.haskell.org/package/QuickCheck) is a dependency of your test-suite.

```haskell
<interactive>:39:3:
    Not in scope: ‘polyQuickCheck’
    In the splice: $(polyQuickCheck (mkName "doctest_prop"))

<interactive>:39:3:
    GHC stage restriction:
      ‘polyQuickCheck’ is used in a top-level splice or annotation,
      and must be imported, not defined locally
    In the expression: polyQuickCheck (mkName "doctest_prop")
    In the splice: $(polyQuickCheck (mkName "doctest_prop"))
```

## Hiding examples from Haddock

You can put examples into [named chunks][named-chunks], and not refer to them
in the export list.  That way they will not be part of the generated Haddock
documentation, but Doctest will still find them.

```haskell
-- $
-- >>> 1 + 1
-- 2
```

[named-chunks]: http://www.haskell.org/haddock/doc/html/ch03s05.html

## Using GHC extensions

You can enable GHC extensions using the following syntax:

```haskell
-- >>> :set -XTupleSections
```

If you want to omit the information which language extensions are enabled from
the Doctest examples you can use the method described in [Hiding examples from
Haddock](#hiding-examples-from-haddock), e.g.:

```haskell
-- $
-- >>> :set -XTupleSections
```

[language-pragma]: http://www.haskell.org/ghc/docs/latest/html/users_guide/pragmas.html#language-pragma

## Using GHC plugins
You can enable GHC plugins using the following syntax:

```haskell
-- >>> :set -fplugin The.Plugin
```

## Hiding Prelude
You _hide_ the import of `Prelude` by using:

```haskell
-- >>> :m -Prelude
```

## Per module options
You can override command line flags per module by using a module annotation. For example, if you know a specific module does not support test order randomization, you can disable it with:

```haskell
{-# ANN module "--no-randomize-order" #-}
```


# Relation to [`doctest`](https://github.com/sol/doctest)
This is a fork of [sol/doctest](https://github.com/sol/doctest) that allows running tests in parallel and aims to provide a more robust project integration method. It is not backwards compatible and expects to be setup differently. At the time of writing it has a few advantages over the base project:

 * It runs tests in parallel
 * It runs tests against compiled code, instead of reinterpreting your whole project
 * It isolates examples in modules, ensuring your tests don't accidentally rely on each other
 * It parses cabal files to discover modules, no need for custom setup anymore!
 * A minor change: it does not count lines in setup blocks as test cases
 * A minor change: the testsuite has been ported to v2 commands

 There are two downsides to using this project:

 * Examples in non-exposed modules cannot be tested (but will nonetheless be detected and consequently fail)
 * Use of conditionals in a cabal file as well as CPP flags will be ignored (TODO?)

All in all, you can expect `doctest-parallel` to run about 1 or 2 orders of magnitude faster than `doctest` for large projects.

# Relation to [`cabal-docspec`](https://github.com/phadej/cabal-extras/tree/master/cabal-docspec)
There is no direct relation between `doctest-parallel` and `cabal-docspec`. They are similar in some ways:

 * Both projects load code from precompiled modules
 * Both project aim to get rid of the need for custom setups

And different in others:

 * As a fork of `doctest`, `doctest-parallel` inherits the testsuite `doctest` accumulated over the years.
 * `doctest-parallel` parses Cabal project files, instead of parsing files from `dist-newstyle`. This makes it compatible with Stack, provided a `.cabal` is still present.
 * `doctest-parallel` uses the GHC API to parse comments. This should in theory be more reliable (though I doubt it will ever matter in practice).
 * `doctest-parallel` runs tests in parallel.

# Development
To run the tests:

```
cabal run spectests
cabal run doctests
```

# Future of this project

 * It would be lovely if we could get rid of the needs for `write-ghc-environment-files: always` option for Cabal. To properly do this, I think Cabal should do two things:
    1. Deprecate GHC environment files as a way to _implicitly_ setup environments. Instead, environment files should be written to the `dist-newstyle` directory and activated using some subcommand, e.g. `cabal shell`. This avoids the many problems GHC environment files have, while retaining their functionality for people who like them.
    2. Any subcommands should be run with `GHC_ENVIRONMENT` set - pointing to the GHC environment file. Like Stack, this would create a hassle free way of using Cabal in combination with projects/executables that use the GHC API (e.g., `clash-ghc`, `doctest-parallel`).
 * It would be nice if Cabal would expose more information _by default_ (probably through auto-generated modules) in order for `doctest-parallel` to properly work. Specifically, it needs to know the exact `default-extensions`, `ghc-options`, and `CPP` flags the project is compiled with. These options are obtainable by using a custom `Setup.hs`, but this has its own list of problems.
   * Alternatively, if comments could be included in and loaded from `.hi` files that'd solve all issues too.
 * Hopefully many of the improvements made here can make their way back into `sol/doctest`.

 Of course, if you wish to add a feature that's not in this list, please feel free top open a pull request!

# Contributors

 * Adam Vogt
 * Anders Persson
 * Ankit Ahuja
 * Edward Kmett
 * Hiroki Hattori
 * Joachim Breitner
 * João Cristóvão
 * Julian Arni
 * Kazu Yamamoto
 * Levent Erkok
 * Luke Murphy
 * Matvey Aksenov
 * Michael Orlitzky
 * Michael Snoyman
 * Nick Smallbone
 * Sakari Jokinen
 * Simon Hengel
 * Sönke Hahn
