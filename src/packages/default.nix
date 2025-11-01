{ inputs, system }:
let
  overlay = (import ../overlays { inherit inputs; }).default;
  config = (import ../overlays { inherit inputs; }).config;
  pkgs = import inputs.nixpkgs {
    inherit system;
    overlays = [
      overlay
      config
    ];
  };
in
{
  swhkd = pkgs.callPackage ./swhkd { };
  ndlm = pkgs.callPackage ./ndlm { };
  vscode-java-debug = pkgs.vscode-java-debug;
  vscode-java-test = pkgs.vscode-java-test;
}
