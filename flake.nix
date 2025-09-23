{
  inputs = {
    configuration.url = "git+file:///home/ivand/src/configuration.nix";
    # nixpkgs
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    # flake-compat to use this flake in configuration.nix
    flake-compat.url = "github:edolstra/flake-compat";
    flake-compat.flake = false;
    # manages the home
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    # nvim config helper
    nixvim.url = "github:nix-community/nixvim";
    nixvim.inputs.nixpkgs.follows = "nixpkgs";
    # neovim latest version
    neovim-nightly-overlay.url = "github:nix-community/neovim-nightly-overlay";
    neovim-nightly-overlay.inputs.nixpkgs.follows = "nixpkgs";
    # for mailserver config
    simple-nixos-mailserver.url = "gitlab:simple-nixos-mailserver/nixos-mailserver";
    simple-nixos-mailserver.inputs.nixpkgs.follows = "nixpkgs";
    # block shitty servers
    hosts.url = "github:StevenBlack/hosts";
    hosts.inputs.nixpkgs.follows = "nixpkgs";
    # for formatting
    treefmt.url = "github:numtide/treefmt-nix";
    treefmt.inputs.nixpkgs.follows = "nixpkgs";
    # for vpsadmin
    vpsadminos.url = "github:vpsfreecz/vpsadminos";
    # my website
    webshite.url = "github:ivandimitrov8080/idimitrov.dev";
    webshite.inputs.nixpkgs.follows = "nixpkgs";
  };
  outputs =
    inputs:
    let
      system = "x86_64-linux";
      pkgs = import inputs.nixpkgs { inherit system; };
    in
    {
      nixosConfigurations = import ./src/configs { inherit inputs; };
      overlays = import ./src/overlays { inherit inputs; };
      devShells = import ./src/shells { inherit inputs; };
      formatter = import ./src/formatter { inherit inputs; };
      packages."x86_64-linux".swhkd = pkgs.callPackage ./src/packages/swhkd { };
    };
}
