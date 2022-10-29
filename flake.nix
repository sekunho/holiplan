{
  description = "A very basic flake";

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, haskellNix, flake-utils }:
    let
      system = "x86_64-linux";

      overlays = [
        haskellNix.overlay (final: prev: {
          holiplan = final.haskell-nix.cabalProject' {
            src = ./.;
            compiler-nix-name = "ghc924";

            shell = {
              # exactDeps = true;
              withHoogle = true;
              withHaddock = true;

              tools = {
                cabal = {};
                hlint = {};
                haskell-language-server = {};
                fourmolu = {};
              };

              buildInputs = with pkgs; [
                watchexec
                sqitchPg
                pkg-config
                zlib
              ];

              shellHook = ''
                alias haskell-language-server-wrapper="haskell-language-server"
              '';
            };
          };
        })
      ];

      pkgs = import nixpkgs {
        inherit system overlays;
        inherit (haskellNix) config;
      };

      pkgs-unstable = nixpkgs.legacyPackages.${system};
      flake = pkgs.holiplan.flake {};
    in flake // {
      packages.${system} = rec {
        default = flake.packages."holiplan:exe:holiplan";
        holiplan = default;
      };

      devShells.${system}.default = pkgs.holiplan.shell;

      nixosModules.${system} = rec {
        holiplan = import ./nix/services/holiplan.nix;
        default = holiplan;
      };
    };
}
