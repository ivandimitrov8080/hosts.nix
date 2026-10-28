{
  inputs = {
    # iso.url = "github:ivandimitrov8080/iso.nix";
    iso.url = "git+file:///home/ivand/src/hosts.nix/iso.nix";
    # nova.url = "github:ivandimitrov8080/nova.nix";
    nova.url = "git+file:///home/ivand/src/hosts.nix/nova.nix";
    # vps.url = "github:ivandimitrov8080/vps.nix";
    vps.url = "git+file:///home/ivand/src/hosts.nix/vps.nix";
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
