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
          (pkgs.nvim.extend {
            plugins = {
              lsp.servers = {
                nushell.enable = true;
              };
            };
          })
        ];
      };
      py = pkgs.mkShell {
        buildInputs = with pkgs; [
          (pkgs.nvim.extend {
            plugins = {
              lsp.servers = {
                pylsp.enable = true;
              };
            };
          })
          python3
        ];
      };
      node = pkgs.mkShell {
        buildInputs = with pkgs; [
          (pkgs.nvim.extend {
            plugins = {
              lsp.servers = {
                ts_ls.enable = true;
              };
            };
          })
          nodejs
        ];
      };
      web = pkgs.mkShell {
        buildInputs = with pkgs; [
          nodejs
          (pkgs.nvim.extend {
            plugins = {
              lsp.servers = {
                ts_ls.enable = true;
                svelte.enable = true;
                html.enable = true;
                cssls.enable = true;
                jsonls.enable = true;
                prismals.enable = true;
                prismals.package = pkgs.npmPackages."@prisma/language-server";
              };
            };
          })
        ];
      };
      rust = pkgs.mkShell {
        buildInputs = with pkgs; [
          rustc
          rust-analyzer
          rustfmt
          cargo
          pkg-config
          (pkgs.nvim.extend {
            plugins = {
              lsp.servers = {
                rust_analyzer = {
                  installCargo = false;
                  installRustc = false;
                };
              };
              rustaceanvim = {
                enable = true;
              };
            };
          })
        ];
      };
      lila = pkgs.mkShell {
        buildInputs = with pkgs; [
          zulu
          coursier
          sbt
          nodejs
          pnpm
          mongodb
          mongosh
          redis
          (pkgs.nvim.extend {
            plugins = {
              lsp.servers = {
                metals.enable = true;
                ts_ls.enable = true;
              };
            };
          })
        ];
      };
    };
}
