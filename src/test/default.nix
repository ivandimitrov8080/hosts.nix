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
    name = "integration-test";
    nodes = {
      dns =
        { pkgs, ... }:
        {
          _module.args.system = system;
          services = {
            dnsmasq = {
              enable = true;
              settings = {
                listen-address = [ "127.0.0.1" ];
                bind-interfaces = true;
                # Provide a deterministic answer
                address = [ "/example.com/203.0.113.10" ];
              };
            };
          };
          environment.systemPackages = extraPkgs;
        };
      vpsfree =
        { pkgs, ... }:
        {
          imports = with nixosModules; [
            configMod
            vpsadminosModule
            mail
            nginx
          ];
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
          environment.etc."wireguard/wg0.priv".source = ./hub.priv;
          meta.wireguard = {
            enable = true;
            address = "10.0.0.1/24";
            privateKeyFile = "/etc/wireguard/wg0.priv";
            peers = pkgs.lib.mkForce peers;
          };
          security.acme.defaults = {
            email = pkgs.lib.mkForce "test@example.com";
            server = pkgs.lib.mkForce "https://acme-staging-v02.api.letsencrypt.org/directory";
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
          environment.etc."wireguard/wg0.priv".source = ./spoke1.priv;
          meta.wireguard = {
            enable = true;
            address = "10.0.0.2/24";
            privateKeyFile = "/etc/wireguard/wg0.priv";
            peers = pkgs.lib.mkForce peers;
          };
          environment.systemPackages = extraPkgs;
        };
      spoke2 =
        { pkgs, ... }:
        {
          imports = [ configMod ];
          environment.etc."wireguard/wg0.priv".source = ./spoke2.priv;
          meta.wireguard = {
            enable = true;
            address = "10.0.0.3/24";
            privateKeyFile = "/etc/wireguard/wg0.priv";
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

        nova.succeed("ping -c1 10.0.0.1")
        spoke2.succeed("ping -c1 10.0.0.1")
        vpsfree.succeed("ping -c1 10.0.0.2")
        vpsfree.succeed("ping -c1 10.0.0.3")

        nova.succeed("nslookup example.com 10.0.0.1")
        nova.fail("nslookup example.com dns")

        nova.succeed("curl http://vpsfree | grep -o '301'")
        nova.succeed("curl -k https://vpsfree | grep -o 'Home | idimitrov.dev'")
      '';
  };
}
// inputs.self.packages.${system}
// inputs.self.devShells.${system}
