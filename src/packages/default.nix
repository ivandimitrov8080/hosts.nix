{ inputs, stdenv }:
let
  overlay = (import ../overlays { inherit inputs; }).default;
  inherit ((import ../overlays { inherit inputs; })) config;
  pkgs = import inputs.nixpkgs {
    inherit (stdenv.hostPlatform) system;
    overlays = [
      overlay
      config
    ];
  };
in
{
  inherit (pkgs) vscode-java-debug;
  inherit (pkgs) vscode-java-test;
}
