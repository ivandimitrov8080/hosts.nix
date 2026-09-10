{ inputs }:
let
  system = "x86_64-linux";
in
{
  default =
    let
      dns = "10.0.0.1";
      peers = [
        {
          PublicKey = "iRSHYRPRELX8lJ2eHdrEAwy5ZW8f5b5fOiIGhHQwKFg=";
          AllowedIPs = [
            "0.0.0.0/0"
            "10.0.0.1/32"
          ];
          Endpoint = "37.205.13.29:51820";
          PersistentKeepalive = 7;
        }
        {
          PublicKey = "rZJ7mJl0bmfWeqpUalv69c+TxukpTaxF/SN+RyxklVA=";
          AllowedIPs = [ "10.0.0.2/32" ];
        }
        {
          PublicKey = "IDe1MPtS46c2iNcE+VrOSUpOVGMXjqFl+XV5Z5U+DDI=";
          AllowedIPs = [ "10.0.0.5/32" ];
        }
      ];
      wirelessNetworks = {
        "3G".psk = "bumshakalaka";
      };
      # TODO: make something similar for vps where it can also send dns traffic back to wireguard peers
      blockDnsExceptDnscrypt = ''
        table inet filter {
          chain output {
            type filter hook output priority 0; policy accept;

            meta oifname "lo" accept

            # Allow dns to server
            ip daddr ${dns} udp dport 53 accept
            ip daddr ${dns} tcp dport 53 accept

            # Block DNS to anywhere else
            udp dport 53 drop
            tcp dport 53 drop
            tcp dport 853 drop
          }
        }
      '';
    in
    {
      default =
        { pkgs, ... }:
        {
          imports = with inputs; [
            configuration.nixosModules.default
          ];
          nix.registry = {
            self.flake = inputs.self;
            nixpkgs.flake = inputs.nixpkgs;
            p.flake = inputs.nixpkgs;
          };
          nixpkgs = {
            hostPlatform = system;
            overlays = [
              inputs.self.overlays.default
              inputs.self.overlays.config
              inputs.self.overlays.emacs
            ];
          };
          system.stateVersion = pkgs.lib.trivial.release;
          users.defaultUserShell = pkgs.nushell;
        };
      wg = _: {
        meta.wireguard = {
          inherit peers dns;
        };
        networking.nameservers = [ dns ];
        networking.nftables = {
          enable = true;
          ruleset = blockDnsExceptDnscrypt;
        };
        services.resolved = {
          settings = {
            Resolve = {
              FallbackDNS = [ dns ];
            };
          };
        };
      };
      minimal =
        { pkgs, ... }:
        {
          imports = with inputs; [
            home-manager.nixosModules.default
          ];
          nix = {
            settings = {
              substituters = [
                "https://nix-community.cachix.org"
                "https://cache.nixos.org/"
              ];
              trusted-public-keys = [
                "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
              ];
            };
          };
          environment.systemPackages = with pkgs; [
            transmission_4
            ffmpeg
            gcc
          ];
          fonts = {
            fontDir.enable = true;
            packages = with pkgs; [
              nerd-fonts.fira-code
              nerd-fonts.symbols-only
              noto-fonts
              noto-fonts-color-emoji
              noto-fonts-lgc-plus
              emacs-all-the-icons-fonts
              inter
            ];
          };
          programs = {
            git.enable = true;
            zoxide.enable = true;
            zsh.enable = true;
            nix-ld.enable = true;
            nix-ld.libraries = with pkgs; [
              alsa-lib
              at-spi2-atk
              cairo
              cups
              dbus
              expat
              glib
              gtk3
              libgbm
              libx11
              libxcb
              libxcomposite
              libxdamage
              libxext
              libxfixes
              libxkbcommon
              libxrandr
              libxrender
              libxtst
              libxi
              freetype
              vulkan-loader
              libGL
              nspr
              nss
              pango
            ];
          };
          services = {
            pipewire.enable = true;
            dbus.enable = true;
            locate.enable = true;
            resolved.enable = true;
            guix.enable = true;
            libretranslate.enable = true;
            kmonad = {
              enable = true;
              keyboards.laptop = {
                device = "/dev/input/by-path/platform-i8042-serio-0-event-kbd";
                defcfg.enable = true;
                config = ''
                  (defsrc
                    esc  f1   f2   f3   f4   f5   f6   f7   f8   f9   f10  f11  f12  ssrq pause ins del
                    grv  1    2    3    4    5    6    7    8    9    0    -    =    bspc
                    tab  q    w    e    r    t    y    u    i    o    p    [    ]
                    caps a    s    d    f    g    h    j    k    l    ;    '    \    ret
                    lsft z    x    c    v    b    n    m    ,    .    /    rsft       pgup up   pgdn
                    lctl lmet lalt           spc                 ralt rctl            left down rght
                  )
                  (deflayer base
                    esc  f1   f2   f3   f4   f5   f6   f7   f8   f9   f10  f11  f12  ssrq pause ins del
                    grv  1    2    3    4    5    6    7    8    9    0    -    =    bspc
                    tab  q    w    e    r    t    y    u    i    o    p    [    ]
                    caps a    s    d    f    g    h    j    k    l    ;    '    \    ret
                    lsft z    x    c    v    b    n    m    ,    .    /    rsft       left up   right
                    lctl lmet lalt           spc                 ralt rctl            left down rght
                  )
                '';
              };
            };
          };
          hardware.uinput.enable = true;
          meta = {
            graphicalBoot.enable = true;
            shells.enable = true;
            swayland.enable = true;
          };
        };
      iso =
        {
          pkgs,
          lib,
          modulesPath,
          ...
        }:
        {
          imports = [
            (modulesPath + "/installer/cd-dvd/iso-image.nix")
          ];
          services.openssh.settings.PermitRootLogin = "yes";
          boot.kernelPackages = lib.mkForce pkgs.linuxPackages;
          home-manager = {
            backupFileExtension = "bak";
            useUserPackages = true;
            useGlobalPkgs = true;
            users.iso = lib.mkMerge [
              (import ./home-manager { inherit inputs pkgs; })
              {
                home = {
                  username = "iso";
                  homeDirectory = "/home/iso";
                  pointerCursor.enable = true;
                };
              }
            ];
          };
          users.users.iso = {
            isNormalUser = true;
            createHome = true;
            shell = pkgs.nushell;
            extraGroups = [
              "adbusers"
              "adm"
              "audio"
              "bluetooth"
              "dialout"
              "input"
              "kvm"
              "mlocate"
              "realtime"
              "render"
              "video"
              "wheel"
            ];
            password = "iso";
          };
        };
      rest =
        { pkgs, lib, ... }:
        {
          home-manager = {
            backupFileExtension = "bak";
            useUserPackages = true;
            useGlobalPkgs = true;
            users.ivand = lib.mkMerge [
              (import ./home-manager/accounts.nix { inherit inputs pkgs; })
              ({ config, ... }: {
                imports = [
                  (import ./home-manager { inherit inputs pkgs config; })
                ];
                home = {
                  username = "ivand";
                  homeDirectory = "/home/ivand";
                  pointerCursor.enable = true;
                };
                programs = {
                  ssh.settings = {
                    vpsfree-ivand = {
                      HostName = "10.0.0.1";
                      User = "ivand";
                    };
                  };
                  git = {
                    settings = {
                      user.name = "Ivan Kirilov Dimitrov";
                      user.email = "ivan@idimitrov.dev";
                    };
                    signing.key = "ED7A E641 69C1 DB37 F48D  68A7 1C27 6C0A 3909 B508";
                  };
                };
              })
            ];
          };
          hardware.bluetooth.enable = true;
          time.timeZone = "UTC";
          environment.systemPackages = with pkgs; [
            simplex-chat-desktop
          ];
          users = {
            users = {
              ivand = {
                isNormalUser = true;
                createHome = true;
                shell = pkgs.nushell;
                extraGroups = [
                  "adbusers"
                  "adm"
                  "audio"
                  "bluetooth"
                  "dialout"
                  "input"
                  "kvm"
                  "mlocate"
                  "realtime"
                  "render"
                  "video"
                  "wheel"
                ];
                hashedPassword = "$y$j9T$Wf9ljhi4c.LUoX/LJEll//$cTP..D/lBWq1PPCzaHhym8V.cibPTjy2JvRYLTf5SZ7";
              };
            };
            extraGroups = {
              realtime = { };
            };
          };
          zramSwap.enable = true;
          systemd.network.networks = {
            "10-wlp45s0" = {
              matchConfig.Name = "wlp45s0";
              networkConfig.DHCP = "yes";
              dhcpV4Config.UseDNS = false;
              dhcpV6Config.UseDNS = false;
              ipv6AcceptRAConfig.UseDNS = false;
            };
            "10-enp47s0" = {
              matchConfig.Name = "enp47s0";
              networkConfig.DHCP = "yes";
              dhcpV4Config.UseDNS = false;
              dhcpV6Config.UseDNS = false;
              ipv6AcceptRAConfig.UseDNS = false;
            };
          };
          networking = {
            hostName = "nova";
            firewall.interfaces.wg0 = {
              allowedTCPPorts = [
                26969 # linking port
              ];
              allowedUDPPorts = [
                26969 # linking port
              ];
            };
            wireless = {
              enable = true;
              networks = wirelessNetworks;
            };
            stevenblack = {
              enable = true;
              block = [
                "fakenews"
                "gambling"
              ];
            };
          };
          meta.wireguard = {
            enable = true;
            address = "10.0.0.2/24";
          };
          programs.java.enable = true;
        };
      vpsadminosModule =
        {
          pkgs,
          lib,
          config,
          ...
        }:
        let
          inherit (lib) mkForce;
        in
        {
          imports = with inputs; [
            simple-nixos-mailserver.nixosModules.default
          ];
          networking.hostName = "vpsfree";
          meta = {
            dnscrypt.enable = true;
            mail.enable = true;
            bingwp.enable = true;
            wireguard = {
              enable = true;
              address = "10.0.0.1/24";
            };
            grafana = {
              enable = true;
              domain = "grafana.idimitrov.dev";
            };
          };
          documentation = {
            enable = false;
            doc.enable = false;
            info.enable = false;
            man.enable = false;
            nixos.enable = false;
          };
          nix.registry = mkForce { };
          services.nginx.enable = true;
          networking = {
            nftables.enable = true;
            firewall = {
              trustedInterfaces = [ "wg0" ];
              allowedTCPPorts = mkForce [
                25 # smtp auth disabled here
                80 # http
                443 # https
              ];
              allowedUDPPorts = mkForce [
                51820 # wireguard
              ];
            };
          };
          users = {
            users = {
              ivand = lib.mkForce {
                isNormalUser = true;
                hashedPassword = "$2b$05$hPrPcewxj4qjLCRQpKBAu.FKvKZdIVlnyn4uYsWE8lc21Jhvc9jWG";
                extraGroups = [
                  "wheel"
                  "adm"
                  "mlocate"
                  "ssh"
                ];
                openssh.authorizedKeys.keys = [
                  ''
                    ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICcLkzuCoBEg+wq/H+hkrv6pLJ8J5BejaNJVNnymlnlo ivan@idimitrov.dev
                  ''
                ];
              };
            };
            groups = {
              ssh = { };
            };
          };
          programs.git.enable = true;
          services = {
            openssh = {
              enable = true;
              settings.ListenAddress = "10.0.0.1";
            };
            radicale = {
              enable = true;
              settings = {
                server = {
                  hosts = [ "127.0.0.1:5232" ];
                };
                auth =
                  let
                    inherit (lib) concatStrings flip mapAttrsToList;
                    htpasswd = pkgs.writeText "radicale.users" (
                      concatStrings (
                        flip mapAttrsToList config.mailserver.accounts (mail: user: mail + ":" + user.hashedPassword + "\n")
                      )
                    );
                  in
                  {
                    type = "htpasswd";
                    htpasswd_filename = "${htpasswd}";
                    htpasswd_encryption = "bcrypt";
                  };
                storage = {
                  filesystem_folder = "/var/lib/radicale/collections";
                };
              };
            };
            postgresql.enable = true;
            dnscrypt-proxy.settings.cloaking_rules = "/etc/dnscrypt-proxy/cloaking_rules.txt";
          };
          environment = {
            enableAllTerminfo = true;
            etc."dnscrypt-proxy/cloaking_rules.txt".text = ''
              *.idimitrov.dev 10.0.0.1
            '';
          };
        };
      mail =
        {
          config,
          lib,
          options,
          ...
        }:
        let
          inherit (lib) mkIf mkEnableOption optionalAttrs;
          cfg = config.meta.mail;
          hasMailserver = options ? mailserver;
        in
        {
          options.meta.mail = {
            enable = mkEnableOption "enable mailserver config";
          };
          config = mkIf cfg.enable (
            optionalAttrs hasMailserver {
              mailserver = {
                stateVersion = 3;
                enable = true;
                localDnsResolver = false;
                fqdn = "idimitrov.dev";
                domains = [
                  "idimitrov.dev"
                  "mail.idimitrov.dev"
                ];
                accounts = {
                  "ivan@idimitrov.dev" = {
                    hashedPassword = "$2b$05$vJqQ.9lIoTig62sNKmBSVOJK0BQ6jq.dVZZxFEI3.9yQpb23Xgake";
                    aliases = [ "admin@idimitrov.dev" ];
                  };
                  "security@idimitrov.dev" = {
                    hashedPassword = "$2b$05$vJqQ.9lIoTig62sNKmBSVOJK0BQ6jq.dVZZxFEI3.9yQpb23Xgake";
                  };
                };
                x509 = {
                  certificateFile = "/var/lib/acme/idimitrov.dev/fullchain.pem";
                  privateKeyFile = "/var/lib/acme/idimitrov.dev/key.pem";
                };
                hierarchySeparator = "/";
              };
              services = {
                postfix.settings = {
                  main.smtpd_sasl_auth_enable = lib.mkForce "no";
                  master.submissions_inet.args = [
                    "-o"
                    "smtpd_sasl_auth_enable=yes"
                  ];
                };
                roundcube = rec {
                  enable = true;
                  hostName = "mail.idimitrov.dev";
                  extraConfig = ''
                    $config['imap_host'] = 'ssl://${hostName}:993';
                    $config['smtp_host'] = "ssl://${hostName}:465";
                    $config['smtp_user'] = "%u";
                    $config['smtp_pass'] = "%p";
                  '';
                };
                rspamd.locals."worker-controller.inc".text = ''
                  secure_ip = [ "10.0.0.0/24" ];
                '';
                postgresql.enable = true;
              };
            }
          );
        };
      nginx =
        { pkgs, ... }:
        {
          security = {
            acme = {
              acceptTerms = true;
              defaults.email = "security@idimitrov.dev";
              certs = {
                "idimitrov.dev" = {
                  extraDomainNames = [ "*.idimitrov.dev" ];
                  dnsProvider = "cloudflare";
                  dnsResolver = "1.1.1.1:53";
                  group = "nginx";
                  environmentFile = "/var/lib/acme/env";
                };
              };
            };
          };
          services.nginx =
            let
              tls = {
                enableACME = false;
                forceSSL = true;
                acmeRoot = null;
                sslCertificate = "/var/lib/acme/idimitrov.dev/fullchain.pem";
                sslCertificateKey = "/var/lib/acme/idimitrov.dev/key.pem";
              };
              serveStatic = "try_files ${
                pkgs.lib.strings.concatStringsSep " " (
                  map (x: "$uri.${x}") [
                    "html"
                    "txt"
                    "png"
                    "jpg"
                    "jpeg"
                    "iso"
                  ]
                )
              } $uri $uri/ =404;";
            in
            {
              defaultListenAddresses = [ "10.0.0.1" ];
              additionalModules = with pkgs.nginxModules; [ geoip2 ];
              commonHttpConfig = ''
                log_format json_access escape=json
                  '{'
                    '"time":"$time_iso8601",'
                    '"remote_addr":"$remote_addr",'
                    '"x_forwarded_for":"$proxy_add_x_forwarded_for",'
                    '"request_method":"$request_method",'
                    '"request_uri":"$request_uri",'
                    '"query_string":"$query_string",'
                    '"status":"$status",'
                    '"bytes_sent":"$body_bytes_sent",'
                    '"request_time":"$request_time",'
                    '"host":"$host",'
                    '"server_name":"$server_name",'
                    '"scheme":"$scheme",'
                    '"protocol":"$server_protocol",'
                    '"referer":"$http_referer",'
                    '"user_agent":"$http_user_agent",'
                    '"upstream_addr":"$upstream_addr",'
                    '"upstream_status":"$upstream_status",'
                    '"upstream_response_time":"$upstream_response_time",'
                    '"asn":"$geoip2_asn",'
                    '"as_org":"$geoip2_asorg",'
                    '"geoip_country_code":"$geoip2_country_code"'
                  '}';
                  access_log /var/log/nginx/access.json.log json_access;

                  geoip2 /var/lib/geoip/iptoasn-country-ipv4.mmdb {
                    auto_reload 5m;
                    $geoip2_country_code  default=-  source=$remote_addr country iso_code;
                  }
                  geoip2 /var/lib/geoip/iptoasn-asn-ipv4.mmdb {
                    auto_reload 5m;
                    $geoip2_asn   default=-  autonomous_system_number;
                    $geoip2_asorg default=-  autonomous_system_organization;
                  }
                  geoip2 /var/lib/geoip/iptoasn-country-ipv6.mmdb {
                    auto_reload 5m;
                    $geoip2_country_code  default=-  source=$remote_addr country iso_code;
                  }
                  geoip2 /var/lib/geoip/iptoasn-asn-ipv6.mmdb {
                    auto_reload 5m;
                    $geoip2_asn   default=-  autonomous_system_number;
                    $geoip2_asorg default=-  autonomous_system_organization;
                  }
                  proxy_headers_hash_max_size 1024;
                  proxy_headers_hash_bucket_size 128;
              '';
              virtualHosts = {
                "idimitrov.dev" = tls // {
                  serverAliases = [ "www.idimitrov.dev" ];
                  listenAddresses = [
                    "10.0.0.1"
                    "37.205.13.29"
                  ];
                  locations."/" = {
                    root = inputs.webshite.packages.${system}.default;

                    extraConfig = ''
                      autoindex on;
                      ${serveStatic}
                    '';
                  };
                };
                "pic.idimitrov.dev" = tls // {
                  listenAddresses = [
                    "10.0.0.1"
                    "37.205.13.29"
                  ];
                  locations."/" = {
                    root = "/var/pic";
                    extraConfig = ''
                      autoindex on;
                    '';
                  };
                };
                "metronome.idimitrov.dev" = tls // {
                  listenAddresses = [
                    "10.0.0.1"
                    "37.205.13.29"
                  ];
                  locations."/" = {
                    root = inputs.metronome.packages.${system}.default;

                    extraConfig = ''
                      autoindex on;
                    '';
                  };
                };
                "grafana.idimitrov.dev" = tls // {
                  locations."/" = {
                    proxyPass = "http://127.0.0.1:34321";
                    proxyWebsockets = true;
                  };
                };
                "mail.idimitrov.dev" = tls;
                "dav.idimitrov.dev" = tls // {
                  locations."/" = {
                    proxyPass = "http://127.0.0.1:5232/";
                    extraConfig = ''
                      proxy_set_header Host $host;
                      proxy_set_header X-Forwarded-Proto $scheme;
                      proxy_set_header  X-Forwarded-For $proxy_add_x_forwarded_for;
                      proxy_pass_header Authorization;
                    '';
                  };
                  locations."= /.well-known/caldav".return = "301 https://dav.idimitrov.dev/";
                  locations."= /.well-known/carddav".return = "301 https://dav.idimitrov.dev/";
                };
                "rspamd.idimitrov.dev" = tls // {
                  locations."/".proxyPass = "http://unix:/run/rspamd/worker-controller.sock:/";
                };
              };
            };
          systemd = {
            services.update-geoip =
              let
                outDir = "/var/lib/geoip";
                url = "https://github.com/sapics/ip-location-db/releases/download/latest";
                exec =
                  pkgs.writers.writeNuBin "exec"
                    # nu
                    ''
                      http get --raw --max-time 69min $"${url}/iptoasn-asn-ipv4.mmdb" | save --raw -f $"${outDir}/iptoasn-asn-ipv4.mmdb"
                      http get --raw --max-time 69min $"${url}/iptoasn-country-ipv4.mmdb" | save --raw -f $"${outDir}/iptoasn-country-ipv4.mmdb"
                      http get --raw --max-time 69min $"${url}/iptoasn-asn-ipv6.mmdb" | save --raw -f $"${outDir}/iptoasn-asn-ipv6.mmdb"
                      http get --raw --max-time 69min $"${url}/iptoasn-country-ipv6.mmdb" | save --raw -f $"${outDir}/iptoasn-country-ipv6.mmdb"
                    '';
              in
              {
                description = "Update GEOIP database from https://github.com/sapics/ip-location-db";
                after = [ "network-online.target" ];
                wants = [ "network-online.target" ];
                serviceConfig = {
                  Type = "oneshot";
                  ReadWritePaths = [ outDir ];
                  ExecStart = "${exec}/bin/exec";
                };
              };
            timers = {
              update-geoip = {
                wantedBy = [ "timers.target" ];
                timerConfig = {
                  OnCalendar = "*-*-* 10:00:00";
                  Persistent = true;
                };
              };
            };
          };
        };
    };
}
