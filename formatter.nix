{ inputs }:
let
  system = "x86_64-linux";
  pkgs = import inputs.nixpkgs { inherit system; };
  treefmt =
    pkgs.treefmt
    // (pkgs.writers.writeNuBin "treefmt"
      {
        makeWrapperArgs = [
          "--prefix"
          "PATH"
          ":"
          "${pkgs.lib.makeBinPath [
            pkgs.git
            pkgs.treefmt
          ]}"
        ];
      }
      ''
        def --wrapped main [...rest] { git submodule foreach treefmt ...$rest }
      ''
    );
in
{
  ${system} =
    (inputs.treefmt.lib.evalModule pkgs {
      projectRootFile = "flake.nix";
      package = treefmt;
      programs = {
        nixfmt.enable = true;
        prettier.enable = true;
        prettier.package = pkgs.prettier;
        deadnix.enable = true;
        statix.enable = true;
      };
      settings = {
        formatter = {
          "nufmt" = {
            command = "${pkgs.nufmt}/bin/nufmt";
            includes = [ "*.nu" ];
          };
        };
      };
    }).config.build.wrapper;
}
