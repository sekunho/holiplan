{
  description = "A very basic flake";

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, haskellNix, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        overlays = [ haskellNix.overlay (final: prev: {
          holiplan = final.haskell-nix.project' {
            src = ./.;
            compiler-nix-name = "ghc924";

            shell.tools = {
              cabal = {};
              hlint = {};
              haskell-language-server = {};
            };

            shell.buildInputs = with pkgs; [
              watchexec
              sqitchPg
              pkg-config
              zlib
              postgresql
            ];
          };
          })
        ];
        pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
        flake = pkgs.holiplan.flake {};

        buildDockerImage = tag: pkgs.dockerTools.buildImage {
          name = "holiplan-docker";
          tag = tag;
          copyToRoot = pkgs.buildEnv {
            name = "image-root";
            paths = [ pkgs.bash ];
            pathsToLink = [ "/bin" ];
          };

          config = {
            Cmd = [ "${self.packages.x86_64-linux.holiplan}/bin/holiplan" ];
            WorkingDir = "/app";
            Env = [ "PATH=${pkgs.coreutils}/bin/:${self.packages.${system}.holiplan}/bin" ];

            ExposedPorts = {
              "8082/tcp" = {};
            };
          };
        };
      in flake // {
        packages = rec {
          default = flake.packages."holiplan:exe:holiplan";
          holiplan = default;
          holiplan-docker = buildDockerImage "latest";
        };
      });
}
