with (import <nixpkgs> {}).pkgs;
let hspkgs = haskell-ng.packages.ghc7101.override {
     overrides = self: super: {
       hmacaroons = self.callPackage ./. {};
      };
   };
in
  hspkgs.hmacaroons.env
