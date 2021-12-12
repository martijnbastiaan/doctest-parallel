#!/usr/bin/env bash

set -xeo pipefail

REPO="ghcr.io/martijnbastiaan"
NAME="focal-ghc-"
DIR=$(dirname "$0")
now=$(date +%F)

GHC_VERSIONS=(  "9.2.1"   "9.0.1"   "8.10.7"  "8.8.4"   "8.6.5"   "8.4.4"  )
CABAL_VERSIONS=("3.6.2.0" "3.4.0.0" "3.2.0.0" "3.2.0.0" "3.0.0.0" "2.4.1.0")

for i in "${!GHC_VERSIONS[@]}"
do
  GHC_VERSION="${GHC_VERSIONS[i]}"
  CABAL_VERSION="${CABAL_VERSIONS[i]}"

  docker build \
    --build-arg cabal_version=${CABAL_VERSION} \
    --build-arg ghc_version=${GHC_VERSION} \
    -t "${REPO}/${NAME}${GHC_VERSION}:$now" \
    -t "${REPO}/${NAME}${GHC_VERSION}:latest" \
    "$DIR"
done

read -p "Push to GitHub? (y/N) " push

if [[ $push =~ ^[Yy]$ ]]; then
  for i in "${!GHC_VERSIONS[@]}"
  do
    GHC_VERSION="${GHC_VERSIONS[i]}"
    docker push "${REPO}/${NAME}${GHC_VERSION}:$now"
    docker push "${REPO}/${NAME}${GHC_VERSION}:latest"
  done
else
  echo "Skipping push to container registry"
fi
