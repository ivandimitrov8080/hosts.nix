{ inputs }:
{
  default =
    final: prev: with prev; {
      which-key = callPackage ./packages/which-key { };
      docs-nixos = inputs.self.nixosConfigurations.nova.config.system.build.manual.optionsJSON;
      docs-hm = inputs.home-manager.packages.${prev.stdenv.hostPlatform.system}.docs-json;
      emacs-custom = callPackage ./packages/emacs { inherit inputs; };
      neomacs = callPackage ./packages/emacs {
        inherit inputs;
        emacs-unstable-pgtk =
          inputs.neomacs.packages.${prev.stdenv.hostPlatform.system}.default.overrideAttrs
            (old: {
              meta.platforms = final.emacs-unstable-pgtk.meta.platforms;
              passthru = (old.passthru or { }) // {
                withTreeSitter = true;
              };
              postInstall = (old.postInstall or "") + ''
                mkdir -p "$out/nix-support"
                cat ${final.emacs-unstable-pgtk}/nix-support/setup-hook >> "$out/nix-support/setup-hook"
              '';
            });
      };
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
