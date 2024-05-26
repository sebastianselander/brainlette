{
  description = "Brainlette the Javalette compiler";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        haskell-packages = nixpkgs.legacyPackages.${system}.haskell.packages;
        ghcVersion = "ghc948";
        pkgs = import nixpkgs { inherit system; };
      in
      {
        packages = {
          default = haskell-packages.${ghcVersion}.developPackage {
            root = ./.;
            withHoogle = true;
            # Look into managing specific versions of dependencies
          };
        };
        devShells = {
          default = pkgs.mkShell {
            nativeBuildInputs = with pkgs; [
              ghc
              haskell-language-server
              haskellPackages.cabal-install
              haskellPackages.fourmolu
              haskellPackages.hoogle
              llvmPackages_latest.llvm
              lldb
              zlib
              gnumake
              clang
            ];
          };
        };
      });
}
