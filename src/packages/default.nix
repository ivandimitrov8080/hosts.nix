{ inputs, system }:
let
  overlay = (import ../overlays { inherit inputs; }).default;
  inherit ((import ../overlays { inherit inputs; })) emacs config;
  pkgs = import inputs.nixpkgs {
    inherit system;
    overlays = [
      overlay
      config
      emacs
    ];
  };
in
{
  inherit (pkgs) hello twvtodo mobile-config-firefox finalrecon xsstrike emacs-custom;
}
