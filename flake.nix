{
  description = "flake that provides ghcicabal";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }: let
    over = final: prev: rec {
      haskellPackages = (prev.haskellPackages.override { overrides = hself: hsuper: {
        ghcicabal = haskellPackages.callPackage ./default.nix {};
      }; });
      ghcicabal = haskellPackages.ghcicabal;
    };
  in flake-utils.lib.eachDefaultSystem (
    system: let
      pkgs = import nixpkgs { inherit system; overlays = [ over ]; };
    in {
      devShell = pkgs.haskellPackages.shellFor {
        packages = p: [ p.ghcicabal ];
        withHoogle = true;
        buildInputs = with pkgs; with pkgs.haskellPackages; [ cabal-install implicit-hie haskell-language-server ];
      }; # devShell

      apps = {
        ghcicabal = { type = "app"; program = "${pkgs.haskellPackages.ghcicabal}/bin/ghcicabal"; };
        buildcabal = { type = "app"; program = "${pkgs.haskellPackages.ghcicabal}/bin/buildcabal"; };
      };
      packages = {
        ghcicabal = pkgs.haskellPackages.ghcicabal;
      };
    }
  ) // { overlay = over; };

}
