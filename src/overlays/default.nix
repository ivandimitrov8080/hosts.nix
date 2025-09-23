{ inputs }:
{
  default =
    _final: prev: with prev; {
      nvim =
        (inputs.nixvim.legacyPackages.${system}.makeNixvim inputs.configuration.nixvimConfigs.default)
        .extend
          {
            package = inputs.neovim-nightly-overlay.packages.${system}.default;
          };
      ndlm = callPackage ../packages/ndlm { };
      npmPackages = callPackage ../packages/npmPackages { };
      xin = callPackage ../packages/xin {
        flakePath = "/home/ivand/src/hosts.nix";
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
    };
}
