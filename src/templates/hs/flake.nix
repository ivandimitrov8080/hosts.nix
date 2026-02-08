{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    configuration.url = "github:ivandimitrov8080/configuration.nix";
    systems.url = "github:nix-systems/default";
    # nvim config helper
    nixvim-flake.url = "github:nix-community/nixvim";
    nixvim-flake.inputs.nixpkgs.follows = "nixpkgs";
    # neovim latest version
    neovim-nightly-overlay.url = "github:nix-community/neovim-nightly-overlay";
    neovim-nightly-overlay.inputs.nixpkgs.follows = "nixpkgs";
    devenv.url = "github:cachix/devenv";
    devenv.inputs.nixpkgs.follows = "nixpkgs";
    treefmt-nix.url = "github:numtide/treefmt-nix";
  };
  outputs =
    inputs@{
      nixpkgs,
      configuration,
      systems,
      nixvim-flake,
      neovim-nightly-overlay,
      devenv,
      treefmt-nix,
      ...
    }:
    let
      eachSystem = nixpkgs.lib.genAttrs (import systems);
      mkPkgs = system: import nixpkgs { inherit system; };
      haskellCompiler = pkgs: (pkgs.ghc.withPackages (p: with p; [ ]));
      packages = eachSystem (
        system:
        let
          pkgs = mkPkgs system;
          inherit (pkgs) stdenv;
        in
        {
          default = stdenv.mkDerivation {
            name = "main";
            version = "1.0";
            src = ./.;
            nativeBuildInputs = with pkgs; [
              (haskellCompiler pkgs)
            ];
            buildPhase = ''
              runHook preBuild

              mkdir -p {bin,_cache}
              ghc -threaded -outputdir _cache/build Main.hs -o bin/main

              runHook postBuild
            '';
            installPhase = ''
              runHook preInstall

              mkdir -p $out/
              cp -r bin $out/

              runHook postInstall
            '';
          };
        }
      );
      devShells = eachSystem (
        system:
        let
          nixvim-default = nixvim-flake.legacyPackages.${system}.makeNixvim {
            package = neovim-nightly-overlay.packages.${system}.default;
          };
          pkgs = import nixpkgs {
            inherit system;
            overlays = [
              (_final: _prev: {
                nixvim = nixvim-default;
              })
              configuration.overlays.default
            ];
          };
        in
        {
          default = devenv.lib.mkShell {
            inherit inputs pkgs;
            modules = [
              {
                devenv.root = throw "You must set devenv root";
                devenv.cli.version = "1";
                packages = with pkgs; [
                  (haskellCompiler pkgs)
                  (nixvim.haskell.extend { })
                  watchexec
                ];
                processes = {
                  site.exec = "watchexec -e hs -- devenv tasks run build";
                };
                tasks = {
                  "clean:all" = {
                    exec = "rm -rf bin _cache";
                  };
                  "build:init" = {
                    exec = ''
                      mkdir -p {bin,_cache}/
                    '';
                    before = [
                      "build:all"
                    ];
                  };
                  "build:all" = {
                    exec = "ghc -threaded -outputdir _cache/build Main.hs -o bin/main";
                  };
                };
                git-hooks.hooks = {
                  nixfmt.enable = true;
                  deadnix.enable = true;
                  statix.enable = true;
                  ormolu.enable = true;
                  ormolu.settings.defaultExtensions = [
                    "ImportQualifiedPost"
                  ];
                };
              }
            ];
          };
        }
      );
      formatter = eachSystem (
        system:
        let
          pkgs = import nixpkgs { inherit system; };
        in
        (treefmt-nix.lib.evalModule pkgs {
          projectRootFile = "flake.nix";
          programs = {
            nixfmt.enable = true;
            deadnix.enable = true;
            statix.enable = true;
            ormolu.enable = true;
            ormolu.ghcOpts = [
              "ImportQualifiedPost"
            ];
          };
        }).config.build.wrapper
      );
    in
    {
      inherit
        devShells
        formatter
        packages
        ;
    };
}
