{
  extras = hackage:
    {
      packages = {
        hmacaroons = ./hmacaroons.nix;
        hex = ./.stack-to-nix.cache.0;
        };
      };
  resolver = "lts-16.31";
  modules = [ ({ lib, ... }: { packages = {}; }) { packages = {}; } ];
  }