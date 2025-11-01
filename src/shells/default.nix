{ inputs }:
{
  "x86_64-linux" =
    let
      overlay = (import ../overlays { inherit inputs; }).default;
      config = (import ../overlays { inherit inputs; }).config;
      pkgs = import inputs.nixpkgs {
        system = "x86_64-linux";
        overlays = [
          overlay
          config
        ];
      };
    in
    {
      default = pkgs.mkShell {
        buildInputs = [
          (pkgs.nixvim.main.extend {
            lsp.servers.nushell.enable = true;
          })
        ];
      };
      lua = pkgs.mkShell {
        buildInputs = [
          pkgs.nixvim.lua
        ];
      };
      py = pkgs.mkShell {
        buildInputs = with pkgs; [
          nixvim.python
          python3
        ];
      };
      web = pkgs.mkShell {
        buildInputs = with pkgs; [
          nixvim.web
          nodejs
          yarn
        ];
      };
      rust = pkgs.mkShell {
        buildInputs = with pkgs; [
          nixvim.rust
          cargo
          libudev-zero
          pkg-config
          rust-analyzer
          rustc
          rustfmt
        ];
      };
      lila = pkgs.mkShell {
        buildInputs = with pkgs; [
          (nixvim.scala.extend {
            lsp.servers.ts_ls.enable = true;
          })
          zulu
          coursier
          sbt
          nodejs
          pnpm
          mongodb
          mongosh
          redis
        ];
      };
      haskell = pkgs.mkShell {
        buildInputs = with pkgs; [
          nixvim.haskell
          ghc
        ];
      };
      c = pkgs.mkShell {
        buildInputs = with pkgs; [
          nixvim.c
          gcc
          meson
          ninja
        ];
      };
      java = pkgs.mkShell {
        buildInputs = with pkgs; [
          nixvim.java
          jdk
          maven
          gradle
          spring-boot-cli
        ];
      };
      quarto = pkgs.mkShell (
        let
          pythonPackages =
            ps: with ps; [
              numpy
              jupyter
              matplotlib
              plotly
            ];
        in
        {
          buildInputs = with pkgs; [
            (quarto.override { extraPythonPackages = pythonPackages; })
            (nixvim.python.extend {
              plugins.quarto.enable = true;
              plugins.otter.enable = true;
            })
            (python3.withPackages pythonPackages)
          ];
        }
      );
    };
}
