{ inputs }:
let
  system = "x86_64-linux";
  pkgs = import inputs.nixpkgs { inherit system; };
in
{
  "${system}" = pkgs.treefmt.withConfig {
    runtimeInputs = with pkgs; [
      nixfmt-rfc-style
      shfmt
    ];

    settings = {
      # Log level for files treefmt won't format
      on-unmatched = "info";

      # Configure nixfmt for .nix files
      formatter = {
        nixfmt.command = "nixfmt";
        nixfmt.includes = [ "*.nix" ];
        sh.command = "shfmt";
        sh.options = [ "--write" ];
        sh.includes = [ "*.sh" ];
      };
    };
  };
}
