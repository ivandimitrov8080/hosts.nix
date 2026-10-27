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
    let
      # shared modules, overlays and packages live here and are handed to the
      # host flakes, which only hold the host specific configuration
      src = ./src;
      # every host takes its option docs from nova
      docsConfig = result.nixosConfigurations.nova;
      result = {
        nixosConfigurations = {
          iso = iso.mkHost { inherit src docsConfig; };
          vps = vps.mkHost { inherit src docsConfig; };
          nova = nova.mkHost { inherit src docsConfig; };
        };
      };
    in
    result;
}
