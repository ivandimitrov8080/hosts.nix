{ inputs, system }:
let
  inherit ((import ../overlays.nix { inherit inputs; })) emacs config default;
  pkgs = import inputs.nixpkgs {
    inherit system;
    overlays = [
      default
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
    ladybird
    dnscrypt-proxy
    ;
}
// configs
