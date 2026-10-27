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
    { self, iso, vps, nova }:
    {
      nixosConfigurations =
        iso.nixosConfigurations
        // vps.nixosConfigurations
        // nova.nixosConfigurations;
    };
}
