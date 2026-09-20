{ inputs, system }:
let
  overlay = (import ../overlays.nix { inherit inputs; }).default;
  inherit ((import ../overlays.nix { inherit inputs; })) emacs config;
  pkgs = import inputs.nixpkgs {
    inherit system;
    overlays = [
      overlay
      config
      emacs
    ];
  };
  configs = builtins.mapAttrs (
    _name: cfg: cfg.config.system.build.toplevel
  ) inputs.self.nixosConfigurations;
in
{
  inherit (pkgs)
    hello
    emacs-custom
    docs-nixos
    docs-hm
    ndlm
    notmuch-ics-import
    ;
}
// configs
