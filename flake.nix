{
  description = "A very basic flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
  };

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
      lib = nixpkgs.lib;
    in {
      devShells.${system}.default = pkgs.mkShell rec {
        buildInputs = with pkgs; [
          haskell.packages.ghc8107.cabal-install
          haskell.packages.ghc8107.haskell-language-server
          haskell.compiler.ghc8107

          zlib
        ];

        LD_LIBRARY_PATH = lib.makeLibraryPath buildInputs;
      };
    };
}
