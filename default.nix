{ pkgs ? import ./nix/pkgs.nix
}: pkgs.haskell-nix.stackProject {
  src = pkgs.haskell-nix.cleanSourceHaskell {
    name = "hmacaroons";
    src = ./.;
  };

  # Have a look at CONTRIBUTING.md
  materialized = ./materialized;
  checkMaterialization = true;
}
