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
        vps = vps.nixosConfigurations.default;
        nova = nova.nixosConfigurations.default;
      };

      # Each host flake owns its own checks; this flake only re-exports them.
      checks.x86_64-linux = {
        iso = iso.checks.x86_64-linux.default;
        nova = nova.checks.x86_64-linux.default;
        vps = vps.checks.x86_64-linux.default;
        vps-integration = vps.checks.x86_64-linux.integration;
      };

      formatter = import ./formatter.nix { inherit inputs; };
    };
}
