FROM ubuntu:focal-20210921

ENV DEBIAN_FRONTEND=noninteractive LANG=C.UTF-8 LC_ALL=C.UTF-8 PATH=/root/.ghcup/bin:${PATH}

ARG ghcup_version="0.1.17.2"
ARG ghc_version
ARG cabal_version

RUN apt-get update \
 && apt-get install -y curl zlib1g-dev build-essential libgmp10-dev libtinfo-dev libtinfo5 \
 && apt-get clean \
 && apt-get autoremove -y

RUN curl "https://downloads.haskell.org/~ghcup/$ghcup_version/x86_64-linux-ghcup-$ghcup_version" --output /usr/bin/ghcup \
 && chmod +x /usr/bin/ghcup \
 && ghcup install stack \
 && stack config set install-ghc false --global \
 && stack config set system-ghc  true  --global \
 && ghcup install ghc $ghc_version --set \
 && ghcup install cabal $cabal_version --set
