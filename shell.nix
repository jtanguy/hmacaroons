{ pkgs ? import <nixpkgs> {}, compiler ? "ghc7101" }:
let hspkgs = pkgs.haskell.packages.${compiler}.override {
     overrides = self: super: {
       hmacaroons = self.callPackage ./. {};
      };
   };
in
  hspkgs.hmacaroons.env
