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
    curl
  ];
  testUser = {
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
  };
in
{
  integrationTest = pkgs.testers.runNixOSTest {
    name = "integration-test";
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
          _module.args.system = system;
          networking.interfaces.eth1.ipv4.addresses = [
            {
              address = "37.205.13.29";
              prefixLength = 24;
            }
          ];
          environment.etc."wireguard/wg0.priv".source = ./hub.priv;
          services.dnscrypt-proxy.settings.offline_mode = true;
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
          systemd.tmpfiles.rules = [
            # directory owned by nginx
            "d /var/lib/geoip 0750 nginx nginx -"
            # copy files, owned by nginx (C+ overwrites/updates)
            "C+ /var/lib/geoip/dbip-asn.mmdb 0640 nginx nginx - ${./dbip-asn.mmdb}"
            "C+ /var/lib/geoip/dbip-country.mmdb 0640 nginx nginx - ${./dbip-country.mmdb}"
            "C+ /var/lib/geoip/dbip-city.mmdb 0640 nginx nginx - ${./dbip-city.mmdb}"
          ];
        }
        // testUser;
      nova =
        { pkgs, lib, ... }:
        {
          imports = with nixosModules; [
            configMod
            rest
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
        }
        // testUser;
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
        }
        // testUser;
      dns =
        _:
        {
          _module.args.system = system;
          services = {
            dnsmasq = {
              enable = true;
              settings = {
                listen-address = [ "127.0.0.1" ];
                bind-interfaces = true;
                # Provide a deterministic answer
                address = [ "/idimitrov.dev/37.205.13.29" ];
              };
            };
          };
          environment.systemPackages = extraPkgs;
        }
        // testUser;
      outsider =
        _:
        {
          environment.systemPackages = extraPkgs;
        }
        // testUser;
    };

    testScript =
      # py
      ''
        start_all()
        for m in [nova, vpsfree, spoke2, outsider]:
            m.wait_for_unit("default.target")

        vpsfree.wait_for_unit("grafana.service")
        vpsfree.wait_for_unit("dnscrypt-proxy.service")

        nova.succeed("ping -c1 10.0.0.1")
        spoke2.succeed("ping -c1 10.0.0.1")
        vpsfree.succeed("ping -c1 10.0.0.2")
        vpsfree.succeed("ping -c1 10.0.0.3")

        nova.succeed("nslookup idimitrov.dev 10.0.0.1")
        nova.fail("nslookup idimitrov.dev dns")

        nova.succeed("curl http://idimitrov.dev | grep -o '301'")
        nova.succeed("curl -k https://idimitrov.dev")

        nova.succeed("curl -k https://mail.idimitrov.dev")
        outsider.fail("curl -k https://mail.idimitrov.dev")
        outsider.fail("curl --resolve mail.idimitrov.dev:443:10.0.0.1 -k https://mail.idimitrov.dev")

        nova.succeed("curl -k https://grafana.idimitrov.dev")
        outsider.fail("curl -k https://grafana.idimitrov.dev")
        outsider.fail("curl --resolve grafana.idimitrov.dev:443:10.0.0.1 -k https://grafana.idimitrov.dev")
      '';
  };
}
// inputs.self.packages.${system}
// inputs.self.devShells.${system}
