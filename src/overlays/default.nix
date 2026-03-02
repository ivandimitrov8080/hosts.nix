{ inputs }:
{
  default =
    _final: prev:
    with prev;
    let
      inherit (inputs.nixvim.legacyPackages.${system}) makeNixvim;
    in
    {
      nixvim = makeNixvim {
        package = inputs.neovim-nightly-overlay.packages.${system}.default;
      };
      ndlm = inputs.ndlm.packages.${system}.default;
      npmPackages = callPackage ../packages/npmPackages { };
      which-key = callPackage ../packages/which-key { };
      myMpvScripts = callPackage ../packages/mpvScripts { };
      xin = callPackage ../packages/xin { };
    };
  config = inputs.configuration.overlays.default;
}
