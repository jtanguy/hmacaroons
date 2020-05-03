{ pkgs ? import <nixpkgs> {} }:

let package = import ./ci.nix ;
in package.shellFor { buildInputs = with pkgs.haskellPackages; [ ghcid ]; }
