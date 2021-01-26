let
  haskellNix = import (import ./sources.nix)."haskell.nix" {};
  nixpkgsSrc = haskellNix.sources.nixpkgs-2009;
  nixpkgsArgs = haskellNix.nixpkgsArgs;

in import nixpkgsSrc nixpkgsArgs
