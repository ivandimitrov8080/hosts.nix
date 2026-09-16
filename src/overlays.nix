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
      xin = callPackage ../packages/xin { };
      llm-tool-collection = callPackage ../packages/llm-tool-collection { };
      docs-nixos = inputs.self.nixosConfigurations.nova.config.system.build.manual.optionsJSON;
      docs-hm = inputs.home-manager.packages.${prev.stdenv.hostPlatform.system}.docs-json;
      emacs-custom = callPackage ../packages/emacs { inherit (inputs) emacs-overlay; };
    };
  config = inputs.configuration.overlays.default;
  emacs = inputs.emacs-overlay.overlays.default;
}
