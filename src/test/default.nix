{ inputs, system }:
let
  pkgs = import inputs.nixpkgs {
    inherit system;
    overlays = [
      inputs.self.overlays.default
      inputs.self.overlays.config
    ];
  };
  inherit (pkgs) lib;
  inherit (lib) mkForce;
  nixosModules = inputs.self.nixosModules.default;
  configMod = inputs.configuration.nixosModules.default;
  hubPub = builtins.readFile ./hub.pub;
  spoke1Pub = builtins.readFile ./spoke1.pub;
  spoke2Pub = builtins.readFile ./spoke2.pub;
  peers = [
    {
      PublicKey = hubPub;
      AllowedIPs = [ "${vpsfreeWgIp}/32" ];
      Endpoint = "${vpsfreeInternetIp}:51820";
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
    inetutils
    nmap
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
  vpsfreeWgIp = "10.0.0.1";
  novaWgIp = "10.0.0.2";
  spoke2WgIp = "10.0.0.3";
  vpsfreeInternetIp = "37.205.13.29";
  internetVlan = 10;
  publicVlan = 20;
  internetGw = "192.168.100.1";
  internetCidrPrefix = 24;
  # “internet side” IPs
  dnsInternetIp = "192.168.100.53";
  novaInternetIp = "192.168.100.10";
  spoke2InternetIp = "192.168.100.11";
  outsiderInternetIp = "192.168.100.12";
  # “public side” router IP (same subnet as vpsfreeIp)
  publicGwIp = "37.205.13.1";
in
{
  integrationTest = pkgs.testers.runNixOSTest {
    name = "integration-test";
    nodes = {
      router =
        { lib, ... }:
        {
          virtualisation.vlans = [
            internetVlan
            publicVlan
          ];

          networking = {
            useNetworkd = lib.mkForce true;
            useDHCP = lib.mkForce false;
            enableIPv6 = lib.mkForce false;

            # IMPORTANT: interface numbers assume:
            # eth0 = NAT, eth1 = default test LAN, eth2 = vlan 10, eth3 = vlan 20
            interfaces.eth1.ipv4.addresses = [
              {
                address = internetGw;
                prefixLength = internetCidrPrefix;
              }
            ];
            interfaces.eth2.ipv4.addresses = [
              {
                address = publicGwIp;
                prefixLength = 24;
              }
            ];

            nftables.enable = true;
            firewall.enable = false; # we’ll define forward policy ourselves
            nftables.ruleset = ''
              table inet filter {
                chain forward {
                  type filter hook forward priority 0; policy drop;
                  ct state established,related accept

                  # mimic “real internet”: never forward RFC1918 WG space
                  ip daddr 10.0.0.0/24 drop

                  # allow internet <-> public
                  iifname "eth1" oifname "eth2" accept
                  iifname "eth2" oifname "eth1" accept
                }
              }
            '';
          };
          boot.kernel.sysctl."net.ipv4.ip_forward" = true;
        }
        // testUser;
      vpsfree =
        { pkgs, ... }:
        {
          _module.args.system = system;
          imports = with nixosModules; [
            configMod
            vpsadminosModule
            mail
            nginx
          ];
          virtualisation.vlans = [ publicVlan ];
          networking = {
            useDHCP = false;
            interfaces = {
              eth1.ipv4.addresses = [
                {
                  address = vpsfreeInternetIp;
                  prefixLength = 24;
                }
              ];
              eth1.ipv4.routes = [
                {
                  address = "192.168.100.0";
                  prefixLength = internetCidrPrefix;
                  via = publicGwIp;
                }
              ];
            };
          };
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
          virtualisation.vlans = [ internetVlan ];
          networking.useNetworkd = lib.mkForce true;
          networking.useDHCP = lib.mkForce false;
          networking.enableIPv6 = lib.mkForce false;
          networking.interfaces.eth1.ipv4 = {
            addresses = [
              {
                address = novaInternetIp;
                prefixLength = internetCidrPrefix;
              }
            ];
            routes = [
              {
                address = "37.205.13.0";
                prefixLength = 24;
                via = internetGw;
              }
            ];
          };
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
          virtualisation.vlans = [ internetVlan ];
          networking.useNetworkd = lib.mkForce true;
          networking.useDHCP = lib.mkForce false;
          networking.enableIPv6 = lib.mkForce false;
          networking.interfaces.eth1.ipv4.addresses = [
            {
              address = spoke2InternetIp;
              prefixLength = internetCidrPrefix;
            }
          ];
          networking.interfaces.eth1.ipv4.routes = [
            {
              address = "37.205.13.0";
              prefixLength = 24;
              via = internetGw;
            }
          ];
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
          imports = [
            configMod
          ];
          virtualisation.vlans = [ internetVlan ];
          networking.useNetworkd = lib.mkForce true;
          networking.useDHCP = lib.mkForce false;
          networking.enableIPv6 = lib.mkForce false;
          networking.interfaces.eth1.ipv4.addresses = [
            {
              address = dnsInternetIp;
              prefixLength = internetCidrPrefix;
            }
          ];
          networking.interfaces.eth1.ipv4.routes = [
            {
              address = "37.205.13.0";
              prefixLength = 24;
              via = internetGw;
            }
          ];
          meta.dnscrypt.enable = true;
          services.dnscrypt-proxy.settings = {
            offline_mode = true;
            listen_addresses = [ "${dnsInternetIp}:53" ];
            cloaking_rules = "/etc/dnscrypt-proxy/cloaking_rules.txt";
          };
          environment = {
            systemPackages = extraPkgs;
            etc."dnscrypt-proxy/cloaking_rules.txt".text = ''
              *.idimitrov.dev ${vpsfreeInternetIp}
            '';
          };
          networking = {
            nftables.enable = true;
            firewall = {
              allowedTCPPorts = mkForce [
                53
              ];
              allowedUDPPorts = mkForce [
                53
              ];
            };
          };
        }
        // testUser;
      outsider =
        _:
        {
          virtualisation.vlans = [ internetVlan ];
          networking.interfaces.eth1.ipv4.addresses = [
            {
              address = outsiderInternetIp;
              prefixLength = internetCidrPrefix;
            }
          ];
          networking.interfaces.eth1.ipv4.routes = [
            {
              address = "37.205.13.0";
              prefixLength = 24;
              via = internetGw;
            }
          ];
          environment.systemPackages = extraPkgs;
          networking = {
            useNetworkd = true;
            useDHCP = false;
            enableIPv6 = false;
            nameservers = [
              dnsInternetIp
            ];
          };
          services.resolved.enable = true;
        }
        // testUser;
    };

    testScript =
      # py
      ''
        start_all()
        for m in [router, dns, nova, vpsfree, spoke2, outsider]:
            m.wait_for_unit("default.target")


        for m in [dns, vpsfree]:
            m.wait_for_unit("dnscrypt-proxy.service")

        vpsfree.wait_for_unit("grafana.service")

        vpsfree.wait_until_succeeds("nslookup idimitrov.dev ${vpsfreeWgIp} | grep -F ${vpsfreeWgIp}")
        dns.wait_until_succeeds("nslookup idimitrov.dev ${dnsInternetIp} | grep -F ${vpsfreeInternetIp}")

        nova.succeed("ping -c1 ${vpsfreeWgIp}")
        spoke2.succeed("ping -c1 ${vpsfreeWgIp}")
        vpsfree.succeed("ping -c1 ${novaWgIp}")
        vpsfree.succeed("ping -c1 ${spoke2WgIp}")
        vpsfree.succeed("ip a | grep ${vpsfreeInternetIp}")
        nova.succeed("ping -c1 ${vpsfreeInternetIp}")

        nova.succeed("nslookup idimitrov.dev ${vpsfreeWgIp}")
        nova.fail("nslookup idimitrov.dev ${dnsInternetIp}")
        outsider.succeed("nslookup idimitrov.dev ${dnsInternetIp}")
        outsider.fail("nslookup idimitrov.dev ${vpsfreeWgIp}")

        nova.succeed("curl http://idimitrov.dev | grep -o '301'")
        nova.succeed("curl -k https://idimitrov.dev")

        outsider.succeed("curl -k https://idimitrov.dev")

        nova.succeed("curl -k https://mail.idimitrov.dev | grep 'Roundcube Webmail'")
        outsider.fail("curl -k https://mail.idimitrov.dev | grep 'Roundcube Webmail'")
        outsider.fail("curl --resolve mail.idimitrov.dev:443:${vpsfreeWgIp} -k https://mail.idimitrov.dev | grep 'Roundcube Webmail'")

        nova.succeed("curl -k https://grafana.idimitrov.dev | grep 'Found'")
        outsider.fail("curl -k https://grafana.idimitrov.dev | grep 'Found'")
        outsider.fail("curl --resolve grafana.idimitrov.dev:443:${vpsfreeWgIp} -k https://grafana.idimitrov.dev | grep 'Found'")
      '';
  };
}
// inputs.self.packages.${system}
// inputs.self.devShells.${system}
