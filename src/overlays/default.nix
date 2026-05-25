{ inputs }:
{
  default =
    _final: prev:
    with prev;
    let
      inherit (inputs.nixvim.legacyPackages.${stdenv.hostPlatform.system}) makeNixvim;
    in
    {
      nixvim = makeNixvim {
        package = inputs.neovim-nightly-overlay.packages.${stdenv.hostPlatform.system}.default;
      };
      which-key = callPackage ../packages/which-key { };
      twvtodo = callPackage ../packages/twvtodo { };
      finalrecon = callPackage ../packages/finalrecon { };
      xsstrike = callPackage ../packages/xsstrike { };
      myMpvScripts = callPackage ../packages/mpvScripts { };
      xin = callPackage ../packages/xin { };
      mobile-config-firefox = callPackage ../packages/mobile-config-firefox { };
      emacs-custom = callPackage ../packages/emacs { };
    };
  config = inputs.configuration.overlays.default;
  emacs = inputs.emacs-overlay.overlays.default;
}
