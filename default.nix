{ pkgs ? import ./nix/pkgs.nix
}: pkgs.haskell-nix.stackProject {
  src = pkgs.haskell-nix.cleanSourceHaskell {
    name = "hmacaroons";
    src = ./.;
  };

  materialized = ./materialized;
  checkMaterialization = true;
}
