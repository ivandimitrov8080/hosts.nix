{ inputs }:
{
  default =
    _final: prev: with prev; {
      which-key = callPackage ./packages/which-key { };
      docs-nixos = inputs.self.nixosConfigurations.nova.config.system.build.manual.optionsJSON;
      docs-hm = inputs.home-manager.packages.${prev.stdenv.hostPlatform.system}.docs-json;
      emacs-custom = callPackage ./packages/emacs { inherit inputs; };
      emacsPackagesFor =
        emacs:
        (prev.emacsPackagesFor emacs).overrideScope (
          final: prev:
          (import ./packages/emacsPackages {
            inherit final prev lib;
          })
        );
      notmuch-ics-import = callPackage ./packages/notmuch-ics-import { };
      askpass = callPackage ./packages/askpass { };
    };
  config = inputs.configuration.overlays.default;
  emacs = inputs.emacs-overlay.overlays.default;
}
