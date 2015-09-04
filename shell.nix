{ pkgs ? import <nixpkgs> {}, compiler ? "ghc7102" }:
let hspkgs = pkgs.haskell.packages.${compiler}.override {
  overrides = self: super: {
    this = self.callPackage ./. {};
  };
};
  hsWithTools = pkgs.lib.overrideDerivation hspkgs.this (attrs: with pkgs; {
      buildInputs = [ /* Other deps */ ] ++ attrs.buildInputs;
      });
in
  hsWithTools.env
