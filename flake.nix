{
  inputs = {
    self.submodules = true;
    configuration.url = ./configuration.nix;
    iso.url = ./iso.nix;
    iso.inputs.configuration.follows = "configuration";
    nova.url = ./nova.nix;
    nova.inputs.configuration.follows = "configuration";
    vps.url = ./vps.nix;
    vps.inputs.configuration.follows = "configuration";
    treefmt.url = "github:numtide/treefmt-nix";
    nixpkgs.follows = "treefmt/nixpkgs";
  };
  outputs =
    inputs@{
      iso,
      vps,
      nova,
      ...
    }:
    {
      nixosConfigurations = {
        iso = iso.nixosConfigurations.default;
        vps = iso.nixosConfigurations.default;
        nova = iso.nixosConfigurations.default;
      };
      formatter = import ./formatter.nix { inherit inputs; };
    };
}
