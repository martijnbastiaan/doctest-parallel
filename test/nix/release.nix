let
  pkgs = import ./nix/nixpkgs.nix { };
in
  pkgs.haskellPackages.callPackage ./default.nix { }
