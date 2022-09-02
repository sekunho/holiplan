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
          # Haskal
          haskell.compiler.ghc8107
          haskell.packages.ghc8107.cabal-install

          # Schema migration
          sqitchPg

          # Dev tools
          haskell.packages.ghc8107.haskell-language-server
          haskell.packages.ghc8107.fourmolu
          haskell.packages.ghc8107.implicit-hie
          hlint

          pkg-config
          zlib
          postgresql
        ];

        LD_LIBRARY_PATH = lib.makeLibraryPath buildInputs;
      };
    };
}
