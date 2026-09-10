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
  configs = builtins.mapAttrs (
    _name: cfg: cfg.config.system.build.toplevel
  ) inputs.self.nixosConfigurations;
in
{
  inherit (pkgs)
    hello
    twvtodo
    mobile-config-firefox
    finalrecon
    xsstrike
    emacs-custom
    emigo
    llm-tool-collection
    docs-nixos
    docs-hm
    ;
  inherit configs;
}
