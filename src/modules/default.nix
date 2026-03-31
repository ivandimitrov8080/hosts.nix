{ inputs }:
let
  system = "x86_64-linux";
in
{
  default =
    let
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
          PublicKey = "RqTsFxFCcgYsytcDr+jfEoOA5UNxa1ZzGlpx6iuTpXY=";
          AllowedIPs = [ "10.0.0.3/32" ];
        }
        {
          PublicKey = "1nfOCubuMXC9ZSCvXOIBer9LZoftmXFDFIOia9jr1jY=";
          AllowedIPs = [ "10.0.0.4/32" ];
        }
        {
          PublicKey = "IDe1MPtS46c2iNcE+VrOSUpOVGMXjqFl+XV5Z5U+DDI=";
          AllowedIPs = [ "10.0.0.5/32" ];
        }
      ];
      wirelessNetworks = {
        "Smart-Hostel-2.4".psk = "smarttrans.bg";
        "Yohohostel2.4G".psk = "kaskamaska";
        "Nomado_Guest".psk = "welcomehome";
        "HostelMusala Uni".psk = "mhostelm";
        "BOUTIQUE APARTMENTS".psk = "boutique26";
        "Safestay".psk = "AlldayrooftopBAR";
        "HOSTEL JASMIN 2".psk = "Jasmin2024";
        "HOME".psk = "iloveprague";
        "Vodafone-B925".psk = "7aGh3FE6pN4p4cu6";
        "O2WIFIZ_EXT".psk = "iloveprague";
        "KOTEKLAN_GUEST".psk = "koteklankotek";
        "TP-Link_BE7A".psk = "84665461";
        "Post120".psk = "9996663333";
        "MOONLIGHT2019".psk = "seacrets";
        "Kaiser Terrasse".psk = "Internet12";
        "bumshakalaka".psk = "locomotive420";
        "3G".psk = "bumshakalaka";
        "LeevinGuest".psk = "L33v1nGhF4ro";
        "FaroStation".psk = "riaformosa";
        "blablaloby".psk = "1234512345";
        "blablafloor2".psk = "1234512345";
        "Uspeh".psk = "101076233";
        "Welcome".psk = "slavqnska95";
        "HOTEL-3".psk = "BRIONI2024";
        "The Eye".psk = "11111111";
        "TP-LINK_9F14".psk = "72041543";
        "Largo_Guest".psk = "1122334455";
        "Gore".psk = "1234567890";
        "Jack Sparrow".psk = "11111111";
        "Toncho Mitev".psk = "mazda626";
        "Vivacom_FiberNet-44F2".psk = "d3PtFcg96M";
        "ATHENS-HAWKS" = { };
        "RAMADA-SOFIA" = { };
      };
      # TODO: make something similar for vps where it can also send dns traffic back to wireguard peers
      blockDnsExceptDnscrypt = ''
        table inet filter {
          chain output {
            type filter hook output priority 0; policy accept;

            meta oifname "lo" accept

            # Allow dns to server
            ip daddr 10.0.0.1 udp dport 53 accept
            ip daddr 10.0.0.1 tcp dport 53 accept

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
            overlays = [
              inputs.self.overlays.default
              inputs.self.overlays.config
            ];
          };
          system.stateVersion = "25.11";
          users.defaultUserShell = pkgs.zsh;
        };
      rest =
        { pkgs, ... }:
        {
          imports = with inputs; [
            home-manager.nixosModules.default
          ];
          home-manager = {
            backupFileExtension = "bak";
            useUserPackages = true;
            useGlobalPkgs = true;
            users.ivand =
              { ... }:
              {
                imports = with inputs.configuration.homeManagerModules; [
                  default
                ];
                xdg.enable = true;
                home.packages = with pkgs; [
                  devenv
                  vit
                ];
                programs = {
                  aerc.enable = true;
                  bash.enable = true;
                  bat.enable = true;
                  bottom.enable = true;
                  browserpass.enable = true;
                  delta.enable = true;
                  direnv.enable = true;
                  eza.enable = true;
                  fd.enable = true;
                  firefox.enable = true;
                  fzf.enable = true;
                  gh.enable = true;
                  git.enable = true;
                  gpg.enable = true;
                  imv.enable = true;
                  khal.enable = true;
                  kitty.enable = true;
                  mpv.enable = true;
                  msmtp.enable = true;
                  nix-index.enable = true;
                  password-store.enable = true;
                  pimsync.enable = true;
                  rofi.enable = true;
                  ssh.enable = true;
                  starship.enable = true;
                  swaylock.enable = true;
                  taskwarrior.enable = true;
                  tealdeer.enable = true;
                  tmux.enable = true;
                  waybar.enable = true;
                  yazi.enable = true;
                  zoxide.enable = true;
                  zsh.enable = true;
                  ssh.matchBlocks = {
                    vpsfree-ivand = {
                      hostname = "10.0.0.1";
                      user = "ivand";
                    };
                  };
                  git = {
                    settings = {
                      user.name = "Ivan Kirilov Dimitrov";
                      user.email = "ivan@idimitrov.dev";
                    };
                    signing.key = "C565 2E79 2A7A 9110 DFA7  F77D 0BDA D4B2 11C4 9294";
                  };
                  opencode = {
                    enable = true;
                    agents = {
                      "UI/UX" = ''
                        ---
                        description: Writes consistent and good-lookng web styles
                        mode: primary
                        temperature: 0.1
                        tools:
                          write: true
                          edit: true
                          bash: false
                        ---

                        You are in UI/UX mode. Focus on:

                        - Consistent styles and best practices
                        - Responsive design
                        - Simplicity over complexity
                        - Features over simplicity
                        - Beautiful websites

                        Write beautiful websites without sacrificing simplicity or features. Write to files without
                        asking.
                      '';
                    };
                  };
                  nushell = {
                    enable = true;
                    extraConfig = pkgs.lib.mkAfter ''
                      use ${pkgs.xin}/bin/xin
                    '';
                  };
                };
                services = {
                  gpg-agent.enable = true;
                  wpaperd.enable = true;
                  mako.enable = true;
                  gammastep = {
                    enable = true;
                    latitude = 50.0;
                    longitude = 14.41;
                  };
                  ollama.enable = true;
                };
                wayland.windowManager.sway.enable = true;
                home = {
                  username = "ivand";
                  homeDirectory = "/home/ivand";
                };
                accounts = {
                  calendar = {
                    accounts.ivand = {
                      primary = true;
                      khal = {
                        enable = true;
                        color = "light green";
                      };
                      pimsync = {
                        enable = true;
                        extraRemoteStorageDirectives = [
                          {
                            name = "interval";
                            params = [
                              60
                            ];
                          }
                        ];
                      };
                      remote = {
                        passwordCommand = [
                          "${pkgs.pass}/bin/pass"
                          "vps/mail.idimitrov.dev/ivan@idimitrov.dev"
                        ];
                        type = "caldav";
                        url = "https://dav.idimitrov.dev";
                        userName = "ivan@idimitrov.dev";
                      };
                    };
                  };
                  contact = {
                    accounts.ivand = {
                      khard.enable = true;
                      pimsync.enable = true;
                      remote = {
                        type = "carddav";
                        url = "https://dav.idimitrov.dev";
                        userName = "ivan@idimitrov.dev";
                        passwordCommand = [
                          "${pkgs.pass}/bin/pass"
                          "vps/dav.idimitrov.dev/ivan@idimitrov.dev"
                        ];
                      };
                    };
                  };
                  email = {
                    accounts = {
                      "ivan@idimitrov.dev" = rec {
                        primary = true;
                        realName = "Ivan Kirilov Dimitrov";
                        address = "ivan@idimitrov.dev";
                        userName = address;
                        passwordCommand = "pass vps/mail.idimitrov.dev/ivan@idimitrov.dev";
                        msmtp = {
                          enable = true;
                          extraConfig = {
                            auth = "login";
                          };
                        };
                        signature = {
                          text = ''
                            Ivan Dimitrov
                            Software Developer
                            ivan@idimitrov.dev
                          '';
                        };
                        gpg = {
                          encryptByDefault = true;
                          signByDefault = true;
                          key = "C565 2E79 2A7A 9110 DFA7  F77D 0BDA D4B2 11C4 9294";
                        };
                        smtp = {
                          host = "mail.idimitrov.dev";
                          port = 465;
                          authentication = "login";
                        };
                        imap = {
                          host = "mail.idimitrov.dev";
                          authentication = "login";
                        };
                        aerc = {
                          enable = true;
                          smtpAuth = "login";
                          imapAuth = "auth";
                          extraAccounts = {
                            default = "INBOX";
                            restrict-delete = true;
                          };
                        };
                        offlineimap.enable = true;
                      };
                    };
                  };
                };
              };
          };
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
          hardware.bluetooth.enable = true;
          time.timeZone = "Europe/Prague";
          environment.systemPackages = with pkgs; [
            nixvim.main
            python3
            (pkgs.makeDesktopItem {
              name = "telegram";
              desktopName = "Telegram";
              exec = "env ${pkgs.telegram-desktop}/bin/Telegram -- %U";
              terminal = false;
              icon = "${pkgs.telegram-desktop}/share/icons/hicolor/128x128/apps/org.telegram.desktop.png";
            })
            transmission_4
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
          fonts = {
            fontDir.enable = true;
            packages = with pkgs; [
              nerd-fonts.fira-code
              noto-fonts
              noto-fonts-color-emoji
              noto-fonts-lgc-plus
            ];
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
            nameservers = [ "10.0.0.1" ];
            nftables = {
              enable = true;
              ruleset = blockDnsExceptDnscrypt;
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
          programs = {
            git.enable = true;
            gtklock.enable = true;
            zoxide.enable = true;
            zsh.enable = true;
            nix-ld.enable = true;
          };
          services = {
            pipewire.enable = true;
            dbus.enable = true;
            locate.enable = true;
            resolved = {
              enable = true;
              settings = {
                Resolve = {
                  FallbackDNS = [ "10.0.0.1" ];
                };
              };
            };
          };
          meta = {
            graphicalBoot.enable = true;
            shells.enable = true;
            swayland.enable = true;
            wireguard = {
              enable = true;
              inherit peers;
              address = "10.0.0.2/24";
            };
          };
        };
      gamingModule =
        { pkgs, ... }:
        {
          meta.gaming.enable = true;
          home-manager.users.ivand = {
            wayland.windowManager.sway = {
              config = {
                keybindings = pkgs.lib.mkOptionDefault {
                  "Mod4+o" = "exec ${pkgs.which-key}/bin/which-key";
                };
              };
            };
          };
          environment.systemPackages = with pkgs; [ radeontop ];
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
            simple-nixos-mailserver.nixosModule
          ];
          networking.hostName = "vpsfree";
          meta = {
            shells.enable = true;
            dnscrypt.enable = true;
            mail.enable = true;
            bingwp.enable = true;
            wireguard = {
              enable = true;
              address = "10.0.0.1/24";
              inherit peers;
            };
            grafana = {
              enable = true;
              domain = "grafana.idimitrov.dev";
            };
          };
          services.nginx.enable = true;
          services.postgresql.enable = true;
          networking = {
            nftables = {
              enable = true;
            };
            firewall = {
              interfaces = {
                wg0 = {
                  allowedTCPPorts = mkForce [
                    22 # ssh
                    53 # dns
                    465 # smtps auth enabled here
                    993 # imap
                  ];
                  allowedUDPPorts = mkForce [
                    22 # ssh
                    53 # dns
                    993 # imap
                  ];
                };
              };
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
                    hashedPassword = "$2b$05$rTVIQD98ogXeCBKdk/YufulWHqpMCAlb7SHDPlh5y8Xbukoa/uQLm";
                    aliases = [ "admin@idimitrov.dev" ];
                  };
                  "security@idimitrov.dev" = {
                    hashedPassword = "$2b$05$rTVIQD98ogXeCBKdk/YufulWHqpMCAlb7SHDPlh5y8Xbukoa/uQLm";
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
                  builtins.map (x: "$uri.${x}") [
                    "html"
                    "txt"
                    "png"
                    "jpg"
                    "jpeg"
                  ]
                )
              } $uri $uri/ =404;";
            in
            {
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
                    '"geoip_country_code":"$geoip2_country_code",'
                    '"geoip_country_name":"$geoip2_country_name",'
                    '"geoip_region_name":"$geoip2_region_name",'
                    '"geoip_city_name":"$geoip2_city_name"'
                  '}';
                  access_log /var/log/nginx/access.json.log json_access;

                  geoip2 /var/lib/geoip/dbip-country.mmdb {
                    auto_reload 5m;
                    $geoip2_country_code  default=-  source=$remote_addr country iso_code;
                    $geoip2_country_name  default=-  country names en;
                  }
                  geoip2 /var/lib/geoip/dbip-city.mmdb {
                    auto_reload 5m;
                    $geoip2_city_name     default=-  city names en;
                    $geoip2_region_name   default=-  subdivisions 0 names en;
                  }
                  geoip2 /var/lib/geoip/dbip-asn.mmdb {
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
                  listenAddresses = [ "10.0.0.1" ];
                  locations."/" = {
                    proxyPass = "http://127.0.0.1:34321";
                    proxyWebsockets = true;
                  };
                };
                "mail.idimitrov.dev" = tls // {
                  listenAddresses = [ "10.0.0.1" ];
                };
                "dav.idimitrov.dev" = tls // {
                  listenAddresses = [ "10.0.0.1" ];
                  locations."/" = {
                    proxyPass = "http://127.0.0.1:5232/";
                    extraConfig = ''
                      proxy_set_header Host $host;
                      proxy_set_header X-Forwarded-Proto $scheme;
                      proxy_set_header  X-Forwarded-For $proxy_add_x_forwarded_for;
                      proxy_pass_header Authorization;
                    '';
                  };
                  locations."= /.well-known/caldav".return = "301 https://cal.idimitrov.dev/";
                  locations."= /.well-known/carddav".return = "301 https://cal.idimitrov.dev/";
                };
                "rspamd.idimitrov.dev" = tls // {
                  listenAddresses = [ "10.0.0.1" ];
                  locations."/".proxyPass = "http://unix:/run/rspamd/worker-controller.sock:/";
                };
              };
            };
        };
    };
}
