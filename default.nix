{ nixpkgs ? import nix/nixpkgs.nix {} }:

with nixpkgs.pkgs;
with gitignore;

haskellPackages.callCabal2nix "doctest-parallel" (gitignoreSource ./.) {}
