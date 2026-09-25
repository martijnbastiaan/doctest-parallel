{ nixpkgs ? import ./nix/nixpkgs.nix {} }:
let
  inherit (nixpkgs) pkgs;
  inherit (pkgs) haskellPackages;

  project = import ./release.nix;
in
pkgs.stdenv.mkDerivation {
  name = "shell";
  buildInputs = project.env.propagatedBuildInputs ++ project.env.nativeBuildInputs ++ [
    haskellPackages.cabal-install
  ];
  LC_ALL = "C.UTF-8";

  # All dependencies come from Nix, so give cabal a config without package
  # repositories. Otherwise it tries to bootstrap Hackage, which fails in a
  # pure shell.
  CABAL_CONFIG = builtins.toFile "cabal-config" "";
}
