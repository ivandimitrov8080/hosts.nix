{ inputs }:
let
  system = "x86_64-linux";
  pkgs = import inputs.nixpkgs { inherit system; };
  topiary-nushell = pkgs.fetchFromGitHub {
    owner = "blindFS";
    repo = "topiary-nushell";
    rev = "a922f988062a529c936a4eddc45823396bae0d14";
    hash = "sha256-VnDOj2NIz1/gEkfuCFPaVvOlDcWwBXYudEIwATb53/0=";
  };
in
{
  "${system}" = pkgs.treefmt.withConfig {
    runtimeInputs = with pkgs; [
      nixfmt
      formatjson5
      (topiary.overrideAttrs (
        final: prev: {
          env = {
            TOPIARY_LANGUAGE_DIR = "${topiary-nushell}/languages";
          };
        }
      ))
      gcc
    ];

    settings = {
      # Log level for files treefmt won't format
      on-unmatched = "info";

      # Configure nixfmt for .nix files
      formatter = {
        nixfmt = {
          command = "nixfmt";
          includes = [ "*.nix" ];
          excludes = [ "**/npmPackages/*.nix" ];
        };
        json = {
          command = "formatjson5";
          options = [
            "--replace"
            "--sort_arrays"
            "--no_trailing_commas"
          ];
          includes = [ "*.json" ];
        };
        nu = {
          command = "topiary";
          options = [
            "format"
            "-C"
            "${topiary-nushell}/languages.ncl"
          ];
          includes = [
            "*.nu"
          ];
        };
      };
    };
  };
}
