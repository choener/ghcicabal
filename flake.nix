{
  description = "flake that provides ghcicabal";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-20.09";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }: flake-utils.lib.eachDefaultSystem (
    system: let
      pkgs = import nixpkgs { inherit system; overlays = [ ghcicabal ]; };
      ghcicabal = self: super: {
        ghcicabal = self.haskellPackages.callPackage ./default.nix {};
      };
    in {
      defaultPackage = pkgs.ghcicabal;
      overlay = final: prev: { ghcicabal = final.haskellPackages.callPackage ./default.nix {}; };
    }
  );

}
