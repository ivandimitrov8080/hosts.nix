{ inputs, system }:
let
  pkgs = import inputs.nixpkgs {
    inherit system;
    overlays = [
      inputs.self.overlays.default
      inputs.self.overlays.config
    ];
  };
  nixosModules = inputs.self.nixosModules.default;
  configMod = inputs.configuration.nixosModules.default;
  hubPub = builtins.readFile ./hub.pub;
  spoke1Pub = builtins.readFile ./spoke1.pub;
  spoke2Pub = builtins.readFile ./spoke2.pub;
  peers = [
    {
      PublicKey = hubPub;
      AllowedIPs = [ "10.0.0.1/32" ];
      Endpoint = "vpsfree:51820";
    }
    {
      PublicKey = spoke1Pub;
      AllowedIPs = [ "10.0.0.2/32" ];
    }
    {
      PublicKey = spoke2Pub;
      AllowedIPs = [ "10.0.0.3/32" ];
    }
  ];
  extraPkgs = with pkgs; [
    wireguard-tools
    dig
  ];
in
{
  default = pkgs.testers.runNixOSTest {
    name = "wireguard-integration";
    nodes = {
      vpsfree =
        { pkgs, ... }:
        {
          imports = with nixosModules; [
            configMod
            vpsadminosModule
            mail
            nginx
          ];
          webshite.enable = true;
          _module.args.system = system;
          networking.hosts = {
            "10.0.0.1" = [
              "example.com"
            ];
          };
          users = {
            users = {
              test = {
                isNormalUser = true;
                password = "test";
                extraGroups = [
                  "wheel"
                  "adm"
                  "mlocate"
                  "ssh"
                ];
              };
            };
            groups = {
              ssh = { };
            };
          };
          services = {
            # use dnsmasq to mock dnscrypt-proxy
            dnscrypt-proxy.enable = pkgs.lib.mkForce false;
            dnsmasq = {
              enable = true;
              settings = {
                listen-address = [ "10.0.0.1" ];
                bind-interfaces = true;
                # Provide a deterministic answer
                address = [ "/example.com/203.0.113.10" ];
              };
            };
          };
          meta.wireguard = {
            enable = true;
            address = "10.0.0.1/24";
            privateKeyFile = ./hub.priv;
            peers = pkgs.lib.mkForce peers;
          };
          environment.systemPackages = extraPkgs;
        };
      nova =
        { pkgs, lib, ... }:
        {
          imports = with nixosModules; [
            configMod
            rest
            nova-module
          ];
          boot.consoleLogLevel = lib.mkForce 7;
          networking.useNetworkd = lib.mkForce true;
          meta.wireguard = {
            enable = true;
            address = "10.0.0.2/24";
            privateKeyFile = ./spoke1.priv;
            peers = pkgs.lib.mkForce peers;
          };
          environment.systemPackages = extraPkgs;
        };
      spoke2 =
        { pkgs, ... }:
        {
          imports = [ configMod ];
          meta.wireguard = {
            enable = true;
            address = "10.0.0.3/24";
            privateKeyFile = ./spoke2.priv;
            peers = pkgs.lib.mkForce peers;
          };
          environment.systemPackages = extraPkgs;
        };
    };

    testScript =
      # py
      ''
        start_all()

        nova.wait_for_unit("default.target")
        vpsfree.wait_for_unit("default.target")
        spoke2.wait_for_unit("default.target")

        nova.succeed("nslookup example.com 10.0.0.1")
        nova.fail("nslookup example.com 1.0.0.1")

        nova.succeed("ping -c1 10.0.0.1")
        spoke2.succeed("ping -c1 10.0.0.1")
        vpsfree.succeed("ping -c1 10.0.0.2")
        vpsfree.succeed("ping -c1 10.0.0.3")
      '';
  };
}
