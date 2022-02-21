# dev
  * Command line arguments (such as `--randomize-order`) can now be overridden on a per-module basis ([#25](https://github.com/martijnbastiaan/doctest-parallel/pull/25))
  * Implicit pre-test module imports can now be disabled using `--no-implicit-module-import`. This can help to test functions from non-exposed modules ([#26](https://github.com/martijnbastiaan/doctest-parallel/pull/26))
  * `runModule` does not swallow import errors anymore ([#28](https://github.com/martijnbastiaan/doctest-parallel/issues/28))
  * `autogen-modules` are not searched for tests anymore ([#30](https://github.com/martijnbastiaan/doctest-parallel/issues/30))

# 0.2.1
  * C include directories (Cabal field: `include-dirs`) are now passed to GHC when parsing source files ([#7](https://github.com/martijnbastiaan/doctest-parallel/issues/7))
  * A migration guide has been added ([#11](https://github.com/martijnbastiaan/doctest-parallel/issues/11))
  * Test order can be randomized using `--randomize-order`. Test order can be made deterministic by adding an optional `--seed=N` argument ([#12](https://github.com/martijnbastiaan/doctest-parallel/pull/12))
  * Any non-error output can now be surpressed by `--quiet` ([#20](https://github.com/martijnbastiaan/doctest-parallel/pull/20))
  * Doctest can now be called using a record for option passing in addition to command line arguments. See `mainFromCabalWithConfig` and `mainFromLibraryWithConfig`.

# 0.2
Changes:
  * Support for GHC 9.2 has been added ([#4](https://github.com/martijnbastiaan/doctest-parallel/pull/4))
  * Support for GHC 8.2 has been dropped ([#3](https://github.com/martijnbastiaan/doctest-parallel/pull/3))
  * The dependency `cabal-install-parsers` has been dropped. This trims the dependency tree quite a bit ([#3](https://github.com/martijnbastiaan/doctest-parallel/pull/3))
  * The Hackage distribution now ships all files necessary to run `doctest-parallel`'s tests (Fixes [#1](https://github.com/martijnbastiaan/doctest-parallel/issues/1), PR [#2](https://github.com/martijnbastiaan/doctest-parallel/pull/2))

# 0.1
Fresh fork from `sol/doctest`. See the README for an overview of all the changes.
