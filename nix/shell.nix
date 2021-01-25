# import the sources:
{ pkgs ? import ./pkgs.nix }:

with pkgs;
mkShell { buildInputs = [ niv ]; }
