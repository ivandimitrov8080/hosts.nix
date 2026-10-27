{
  inputs = {
    # iso.url = "github:ivandimitrov8080/iso.nix";
    iso.url = "git+file:///home/ivand/src/hosts.nix/iso.nix";
    # nova.url = "github:ivandimitrov8080/nova.nix";
    nova.url = "git+file:///home/ivand/src/hosts.nix/nova.nix";
    # vps.url = "github:ivandimitrov8080/vps.nix";
    vps.url = "git+file:///home/ivand/src/hosts.nix/vps.nix";
  };
  outputs =
    { iso, vps, nova }:
    {
      # each host flake is self-contained and builds its own configuration;
      # this aggregate just re-exports them under one flake
      nixosConfigurations =
        iso.nixosConfigurations
        // vps.nixosConfigurations
        // nova.nixosConfigurations;
    };
}
