{ inputs }:
let
  system = "x86_64-linux";
  pkgs = import inputs.nixpkgs { inherit system; };
in
{
  "${system}" =
    (inputs.treefmt.lib.evalModule pkgs {
      projectRootFile = "flake.nix";
      programs = {
        nixfmt.enable = true;
        prettier.enable = true;
        deadnix.enable = true;
        statix.enable = true;
        ormolu.enable = true;
        ormolu.ghcOpts = [
          "ImportQualifiedPost"
        ];
      };
      settings.formatter = {
        "nufmt" = {
          command = "${pkgs.bash}/bin/bash";
          options = [
            "-euc"
            ''
              for file in "$@"; do
                ${pkgs.lib.getExe pkgs.nufmt} $file
              done
            ''
            "--" # bash swallows the second argument when using -c
          ];
          includes = [ "*.nu" ];
        };
      };
    }).config.build.wrapper;
}
