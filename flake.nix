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
      nixpkgs,
      ...
    }:
    let
      systems = [ "x86_64-linux" ];

      # Evaluate a host's toplevel without building it: the check only prints
      # the toplevel drvPath (context discarded), so `nix flake check` never
      # builds the system itself.
      evalCheck =
        system: name: cfg:
        nixpkgs.legacyPackages.${system}.runCommand "eval-${name}" { } ''
          echo ${builtins.unsafeDiscardStringContext cfg.config.system.build.toplevel.drvPath} > $out
        '';

      evalDerivation =
        system: name: drv:
        nixpkgs.legacyPackages.${system}.runCommand "eval-${name}" { } ''
          echo ${builtins.unsafeDiscardStringContext drv.drvPath} > $out
        '';

      checksFor = system: {
        iso = evalCheck system "iso" iso.nixosConfigurations.default;
        nova = evalCheck system "nova" nova.nixosConfigurations.default;
        vps = evalCheck system "vps" vps.nixosConfigurations.default;
        # Evaluate the integration test, but do not run the VMs as part of
        # the aggregate check. Run it explicitly with:
        #   nix build ~/src/hosts.nix/vps.nix#checks.x86_64-linux.integration
        vps-integration = evalDerivation system "vps-integration" vps.checks.${system}.integration;
      };
    in
    {
      nixosConfigurations = {
        iso = iso.nixosConfigurations.default;
        vps = vps.nixosConfigurations.default;
        nova = nova.nixosConfigurations.default;
      };

      checks = builtins.listToAttrs (
        map (system: {
          name = system;
          value = checksFor system;
        }) systems
      );

      formatter = import ./formatter.nix { inherit inputs; };
    };
}
