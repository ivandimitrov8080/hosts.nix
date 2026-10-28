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
    let
      system = "x86_64-linux";
      hosts = iso.nixosConfigurations // vps.nixosConfigurations // nova.nixosConfigurations;

      # Evaluation-only check: forces every host's full toplevel derivation
      # without building it, so `nix flake check` reports eval errors from any
      # submodule without needing to know anything about that host.
      evalCheck =
        name: host:
        host.pkgs.runCommand "eval-${name}" { } ''
          echo ${builtins.unsafeDiscardStringContext host.config.system.build.toplevel.drvPath} > $out
        '';
    in
    {
      nixosConfigurations = hosts;
      formatter = import ./formatter.nix { inherit inputs; };
      checks.${system} = builtins.mapAttrs evalCheck hosts;
    };
}
