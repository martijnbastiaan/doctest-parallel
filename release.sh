#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

rm -rf dist-newstyle
rm -rf .ghc.env*

cabal sdist
cabal v2-haddock doctest-parallel \
  --haddock-for-hackage \
  --haddock-hyperlinked-source \
  --enable-documentation

PASSWORD=${PASSWORD:-password-here}
SDIST=$(find . -name 'doctest-parallel-*.tar.gz' | grep -v docs)
DDIST=$(find . -name 'doctest-parallel-*.tar.gz' | grep docs)

echo "To publish a release candidate, run:"
echo "  cabal upload --username=martijnbastiaan --password=${PASSWORD} ${SDIST}"
echo "  cabal upload --documentation --username=martijnbastiaan --password=${PASSWORD} ${DDIST}"
echo ""
echo "To make a release, run:"
echo "  cabal upload --publish --username=martijnbastiaan --password=${PASSWORD} ${SDIST}"
echo "  cabal upload --publish --documentation --username=martijnbastiaan --password=${PASSWORD} ${DDIST}"
