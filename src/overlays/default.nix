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
      myMpvScripts = callPackage ../packages/mpvScripts { };
      xin = callPackage ../packages/xin { };
      python3Packages = prev.python3Packages // {
        fastmcp = prev.python3Packages.fastmcp.overrideAttrs (_: {
          doInstallCheck = false;
        });
      };
    };
  config = inputs.configuration.overlays.default;
}
