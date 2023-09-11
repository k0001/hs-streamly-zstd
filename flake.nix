{
  description = "Haskell 'streamly-zstd' library";

  inputs = {
    nixpkgs.url =
      "github:NixOS/nixpkgs/43297919d746de7b71fc83dba95272b2991ba20f";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs = inputs@{ ... }:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      flake.overlays.default = final: prev: {
        haskell = prev.haskell // {
          packageOverrides = prev.lib.composeExtensions
            (prev.haskell.packageOverrides or (_: _: { })) (hself: hsuper: {
              streamly-zstd = hself.callPackage ./streamly-zstd { };

              streamly-core = hself.callHackage "streamly-core" "0.1.0" { };
              streamly = hself.callHackage "streamly" "0.9.0" { };
            });
        };
      };
      systems = [ "x86_64-linux" "i686-linux" "aarch64-linux" ];
      perSystem = { config, pkgs, system, ... }: {
        _module.args.pkgs = import inputs.nixpkgs {
          inherit system;
          overlays = [ inputs.self.overlays.default ];
        };
        packages = {
          streamly-zstd__ghc94 = pkgs.haskell.packages.ghc94.streamly-zstd;
          # streamly-zstd__ghc96 = pkgs.haskell.packages.ghc96.streamly-zstd;
          default = pkgs.releaseTools.aggregate {
            name = "every output from this flake";
            constituents = [
              config.packages.streamly-zstd__ghc94
              config.packages.streamly-zstd__ghc94.doc
              config.devShells.ghc94

              #config.packages.streamly-zstd__ghc96
              #config.packages.streamly-zstd__ghc96.doc
              #config.devShells.ghc96
            ];
          };
        };
        devShells = let
          mkShellFor = ghc:
            ghc.shellFor {
              packages = p: [ p.streamly-zstd ];
              withHoogle = false;
              nativeBuildInputs =
                [ pkgs.cabal-install pkgs.cabal2nix pkgs.ghcid ];
            };
        in {
          default = config.devShells.ghc94;
          ghc94 = mkShellFor pkgs.haskell.packages.ghc94;
          #ghc96 = mkShellFor pkgs.haskell.packages.ghc96;
        };
      };
    };
}
