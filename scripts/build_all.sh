#!/bin/bash
set -e

GHCS=( "8.10.7" "9.0.2" "9.2.8" "9.4.8" "9.6.6" "9.8.1" "9.10.1" )

cabal update

for GHC in "${GHCS[@]}"
do
  echo "> cabal build all --with-compiler=ghc-$GHC --only-dependencies"
  cabal build all --with-compiler=ghc-$GHC --only-dependencies
done

for GHC in "${GHCS[@]}"
do
  echo "> cabal build all --with-compiler=ghc-$GHC"
  cabal build all --with-compiler=ghc-$GHC
done
