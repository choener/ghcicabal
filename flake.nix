{
  description = "flake that provides ghcicabal";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-20.09";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }: let
    over = final: prev: {
      ghcicabal = final.haskellPackages.callPackage ./default.nix {};
    };
  in flake-utils.lib.eachDefaultSystem (
    system: let
      pkgs = import nixpkgs { inherit system; overlays = [ over ]; };
    in {
      defaultPackage = pkgs.ghcicabal;
    }
  ) // { overlay = over; };

}
