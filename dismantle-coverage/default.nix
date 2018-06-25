{ pkgs ? <nixpkgs>, compiler ? "ghc822" }:
let
   config = {
     packageOverrides = pkgs: with pkgs.haskell.lib; {
       haskell = pkgs.haskell // {
         packages = pkgs.haskell.packages // {
	   ${compiler} = pkgs.haskell.packages.${compiler}.override {
              overrides = self: super: {
                regex = self.callPackage ./regex {};
              };
            };
	  };
        };
      };
    };
  p = import pkgs { inherit config; };
  dismantle-tablegen = p.haskell.packages.${compiler}.callPackage ../dismantle-tablegen/dismantle-tablegen.nix {};
  dismantle-ppc = p.haskell.lib.dontCheck (p.haskell.packages.${compiler}.callPackage ../dismantle-ppc/dismantle-ppc.nix { inherit dismantle-tablegen; });
in
  p.haskell.packages.${compiler}.callPackage ./dismantle-coverage.nix {
    inherit dismantle-tablegen dismantle-ppc;
  }

