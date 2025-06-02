{ inputs, ... }:
{
  perSystem =
    { system, pkgs, ... }:
    {
      config.packages =
        let
          inherit (pkgs) callPackage;
        in
        rec {
          nvim = callPackage ./nvim {
            inherit (inputs.nixvim.legacyPackages.${system}) makeNixvim;
            package = inputs.neovim-nightly-overlay.packages.${pkgs.system}.default;
          };
          xin = pkgs.callPackage ./xin {
            flakePath = "/home/ivand/src/configuration.nix";
            hosts = [
              {
                name = "nova";
                command = "nixos-rebuild";
                subcommand = "switch";
                profile = "nova";
                ref = "nova";
              }
              {
                name = "gaming";
                command = "nixos-rebuild";
                subcommand = "switch";
                profile = "gaming";
                ref = "gaming";
              }
              {
                name = "ai";
                command = "nixos-rebuild";
                subcommand = "switch";
                profile = "ai";
                ref = "ai";
              }
              {
                name = "music";
                command = "nixos-rebuild";
                subcommand = "switch";
                profile = "music";
                ref = "music";
              }
              {
                name = "vps";
                command = "nixos-rebuild";
                subcommand = "switch";
                targetHost = "vpsfree-root";
                ref = "vps";
              }
              {
                name = "stara";
                command = "nixos-rebuild";
                subcommand = "switch";
                targetHost = "stara-root";
                buildHost = "stara-root";
                ref = "stara";
              }
              {
                name = "stara-ai";
                command = "nixos-rebuild";
                subcommand = "switch";
                targetHost = "stara-root";
                buildHost = "stara-root";
                ref = "stara-ai";
              }
            ];
          };
          default = xin;
        };
    };
}
