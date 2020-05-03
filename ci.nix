let
  # Import the Haskell.nix library,
  haskellNix = import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/78f4e25f5c45c135d1798fca423420e8baa7589b.tar.gz) {};
  nixpkgs = import haskellNix.sources.nixpkgs-1909 haskellNix.nixpkgsArgs;
  haskell = nixpkgs.haskell-nix;

  # Instantiate a package set using the generated file.
  pkgSet = haskell.mkStackPkgSet {
    stack-pkgs = (haskell.importAndFilterProject (haskell.callStackToNix {
      src = ./.;
    })).pkgs;
    pkg-def-extras = [
      (hackage: { hsc2hs = hackage.hsc2hs."0.68.4".revisions.default; })
    ];
    # pkg-def-overlays = [];
    modules = [];
  };
in
  pkgSet.config.hsPkgs
