{
  inputs = {
    self.submodules = true;
    nixpkgs.url = ./nixpkgs;
    configuration.url = ./configuration.nix;
    iso = {
      url = ./iso.nix;
      inputs = {
        configuration.follows = "configuration";
        nixpkgs.follows = "nixpkgs";
      };
    };
    nova = {
      url = ./nova.nix;
      inputs = {
        configuration.follows = "configuration";
        nixpkgs.follows = "nixpkgs";
      };
    };
    vps = {
      url = ./vps.nix;
      inputs = {
        configuration.follows = "configuration";
        nixpkgs.follows = "nixpkgs";
      };
    };
    treefmt = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
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

      checks.x86_64-linux = {
        iso = iso.checks.x86_64-linux.default;
        nova = nova.checks.x86_64-linux.default;
        vps = vps.checks.x86_64-linux.default;
      };

      formatter = import ./formatter.nix { inherit inputs; };
    };
}
