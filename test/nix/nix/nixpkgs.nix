{ sources ? import ./../../../nix/sources.nix }:

let
  overlay = _: nixpkgs: {

    # Nix tooling
    niv = (import sources.niv {}).niv;
    gitignore = import sources.gitignore { inherit (nixpkgs) lib; };

    # Haskell overrides
    haskellPackages = nixpkgs.haskellPackages.override {
      overrides = self: super: {
        # External overrides
	# ..no overrides yet

        # Internal overrides
        doctest-parallel = import ../../.. { inherit nixpkgs; };
      };
    };
  };

in import sources.nixpkgs { overlays = [ overlay ]; }

