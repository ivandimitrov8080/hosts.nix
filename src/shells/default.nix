{ inputs }:
{
  "x86_64-linux" =
    let
      overlay = (import ../overlays { inherit inputs; }).default;
      pkgs = import inputs.nixpkgs {
        system = "x86_64-linux";
        overlays = [ overlay ];
      };
    in
    {
      default = pkgs.mkShell {
        buildInputs = [
          (pkgs.nvim.default.extend {
            lsp.servers.nushell.enable = true;
          })
        ];
      };
      lua = pkgs.mkShell {
        buildInputs = [
          pkgs.nvim.lua
        ];
      };
      py = pkgs.mkShell {
        buildInputs = with pkgs; [
          nvim.python
          python3
        ];
      };
      web = pkgs.mkShell {
        buildInputs = with pkgs; [
          nvim.web
          nodejs
          yarn
        ];
      };
      rust = pkgs.mkShell {
        buildInputs = with pkgs; [
          nvim.rust
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
          (nvim.scala.extend {
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
          nvim.haskell
          ghc
        ];
      };
      c = pkgs.mkShell {
        buildInputs = with pkgs; [
          nvim.c
          gcc
          meson
          ninja
        ];
      };
      java = pkgs.mkShell {
        buildInputs = with pkgs; [
          nvim.java
          jdk
          maven
          gradle
          spring-boot-cli
        ];
      };
    };
}
