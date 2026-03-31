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
      myMpvScripts = callPackage ../packages/mpvScripts { };
      xin = callPackage ../packages/xin { };
    };
  config = inputs.configuration.overlays.default;
}
