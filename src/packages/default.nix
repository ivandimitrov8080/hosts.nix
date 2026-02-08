{ inputs, system }:
let
  overlay = (import ../overlays { inherit inputs; }).default;
  inherit ((import ../overlays { inherit inputs; })) config;
  pkgs = import inputs.nixpkgs {
    inherit system;
    overlays = [
      overlay
      config
    ];
  };
in
{
  ndlm = pkgs.callPackage ./ndlm { };
  inherit (pkgs) vscode-java-debug;
  inherit (pkgs) vscode-java-test;
}
