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
        "Smart-Hostel-2.4" = {
          psk = "smarttrans.bg";
        };
        "Yohohostel2.4G" = {
          psk = "kaskamaska";
        };
        "Nomado_Guest" = {
          psk = "welcomehome";
        };
        "HostelMusala Uni" = {
          psk = "mhostelm";
        };
        "BOUTIQUE APARTMENTS" = {
          psk = "boutique26";
        };
        "Safestay" = {
          psk = "AlldayrooftopBAR";
        };
        "HOSTEL JASMIN 2" = {
          psk = "Jasmin2024";
        };
        "HOME" = {
          psk = "iloveprague";
        };
        "Vodafone-B925" = {
          psk = "7aGh3FE6pN4p4cu6";
        };
        "O2WIFIZ_EXT" = {
          psk = "iloveprague";
        };
        "KOTEKLAN_GUEST" = {
          psk = "koteklankotek";
        };
        "TP-Link_BE7A" = {
          psk = "84665461";
        };
        "Post120" = {
          psk = "9996663333";
        };
        "MOONLIGHT2019" = {
          psk = "seacrets";
        };
        "Kaiser Terrasse" = {
          psk = "Internet12";
        };
        "bumshakalaka" = {
          psk = "locomotive420";
        };
        "3G" = {
          psk = "bumshakalaka";
        };
        "LeevinGuest" = {
          psk = "L33v1nGhF4ro";
        };
        "FaroStation" = {
          psk = "riaformosa";
        };
        "blablaloby" = {
          psk = "1234512345";
        };
        "blablafloor2" = {
          psk = "1234512345";
        };
        "Uspeh" = {
          psk = "101076233";
        };
        "Welcome" = {
          psk = "slavqnska95";
        };
        "ATHENS-HAWKS" = { };
        "RAMADA-SOFIA" = { };
      };
      hosts = {
        "10.0.0.1" = [
          "idimitrov.dev"
          "mail.idimitrov.dev"
          "grafana.idimitrov.dev"
        ];
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
        { pkgs, lib, ... }:
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
                ];
                programs = {
                  ssh.matchBlocks = {
                    vpsfree-ivand = {
                      hostname = "10.0.0.1";
                      user = "ivand";
                    };
                    vpsfree-root = {
                      hostname = "10.0.0.1";
                      user = "root";
                    };
                    git = {
                      hostname = "10.0.0.1";
                      user = "git";
                    };
                  };
                  git = {
                    settings = {
                      user.name = "Ivan Kirilov Dimitrov";
                      user.email = "ivan@idimitrov.dev";
                    };
                    signing.key = "C565 2E79 2A7A 9110 DFA7  F77D 0BDA D4B2 11C4 9294";
                  };
                  delta.enable = true;
                  gh.enable = true;
                  password-store.enable = true;
                  opencode = {
                    enable = true;
                    settings = {
                      theme = "catppuccin";
                      model = "copilot/GPT-4.1";
                      autoshare = false;
                      autoupdate = false;
                      mcp = {
                        mcp-nixos = {
                          enabled = true;
                          type = "local";
                          command = [
                            "nix"
                            "run"
                            "github:utensils/mcp-nixos"
                          ];
                        };
                      };
                    };
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
                  direnv = {
                    enable = true;
                    enableZshIntegration = true;
                  };
                  nushell = {
                    enable = true;
                    extraConfig = pkgs.lib.mkAfter ''
                      use ${pkgs.xin}/bin/xin
                    '';
                  };
                  yazi.enable = true;
                  fd.enable = true;
                  ssh.enable = true;
                  gpg.enable = true;
                  git.enable = true;
                  tealdeer.enable = true;
                  bottom.enable = true;
                  fzf.enable = true;
                  nix-index.enable = true;
                  bat.enable = true;
                  bash.enable = true;
                  zsh.enable = true;
                  kitty.enable = true;
                  tmux.enable = true;
                  starship.enable = true;
                  eza.enable = true;
                  zoxide.enable = true;
                  waybar.enable = true;
                  swaylock.enable = true;
                  rofi.enable = true;
                  imv.enable = true;
                  mpv.enable = true;
                  browserpass.enable = true;
                  firefox.enable = true;
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
                  ollama.environmentVariables = {
                    HSA_OVERRIDE_GFX_VERSION = "11.0.2";
                  };
                };
                wayland.windowManager.sway.enable = true;
                home = {
                  username = "ivand";
                  homeDirectory = "/home/ivand";
                };
                accounts = {
                  calendar = {
                    basePath = ".local/share/calendars";
                    accounts.ivand = {
                      primary = true;
                      khal = {
                        enable = true;
                        color = "light green";
                      };
                    };
                  };
                  email = {
                    maildirBasePath = "mail";
                    accounts = {
                      ivan = rec {
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
                        getmail = {
                          enable = true;
                          mailboxes = [ "ALL" ];
                        };
                        gpg = {
                          encryptByDefault = true;
                          signByDefault = true;
                        };
                        smtp = {
                          host = "idimitrov.dev";
                        };
                        imap = {
                          host = "idimitrov.dev";
                        };
                        neomutt = {
                          enable = true;
                          mailboxType = "imap";
                          extraMailboxes = [
                            "Sent"
                            "Drafts"
                            "Trash"
                            "Archive"
                          ];
                        };
                        offlineimap.enable = true;
                      };
                    };
                  };
                };
              };
          };
          i18n.defaultLocale = "en_US.UTF-8";
          time.timeZone = "Europe/Prague";
          systemd.network = {
            wait-online.enable = false;
          };
          environment.systemPackages = with pkgs; [
            audacity
            brightnessctl
            gimp
            grim
            just
            libnotify
            libreoffice-qt
            mupdf
            nixvim.main
            pwvucontrol
            python3
            screenshot
            slurp
            (pkgs.makeDesktopItem {
              name = "telegram";
              desktopName = "Telegram";
              exec = "env ${pkgs.telegram-desktop}/bin/Telegram -- %U";
              terminal = false;
              icon = "${pkgs.telegram-desktop}/share/icons/hicolor/128x128/apps/org.telegram.desktop.png";
            })
            transmission_4
            volume
            wl-clipboard
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
        };
      nova-module =
        { pkgs, lib, ... }:
        {
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
          boot.loader.grub.enable = true;
          boot.loader.grub.efiSupport = false;
          #boot.kernelPackages = pkgs.linuxPackages;
          meta.graphicalBoot.enable = true;
          programs.regreet.enable = lib.mkForce false;
          services.greetd = {
            settings = {
              default_session =
                let
                  greeter = lib.getExe pkgs.ndlm;
                  session = "--session ${pkgs.swayfx}/bin/swayfx";
                  themeFile = "--theme-file /etc/plymouth/themes/catppuccin-mocha/catppuccin-mocha.plymouth";
                in
                {
                  command = lib.mkForce "${greeter} ${session} ${themeFile}";
                  user = "greeter";
                };
            };
          };
          zramSwap.enable = true;
          users.users.greeter = {
            extraGroups = [
              "video"
              "input"
              "render"
            ];
          };
          meta.shells.enable = true;
          meta.swayland.enable = true;
          meta.wireguard = {
            enable = true;
            inherit peers;
            address = "10.0.0.2/24";
          };
          networking.hostName = "nova";
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
            inherit hosts;
            nameservers = [ "10.0.0.1" ];
            nftables = {
              enable = true;
              ruleset = blockDnsExceptDnscrypt;
            };
            wireless = {
              enable = true;
              networks = wirelessNetworks;
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
            resolved = {
              enable = true;
              settings = {
                Resolve = {
                  FallbackDNS = [ "10.0.0.1" ];
                };
              };
            };
          };
          security = {
            sudo = {
              extraRules = [
                {
                  groups = [ "wheel" ];
                  commands = [
                    {
                      command = "${pkgs.brightnessctl}/bin/brightnessctl";
                      options = [ "NOPASSWD" ];
                    }
                  ];
                }
              ];
            };
            polkit.enable = true;
            rtkit.enable = true;
          };
        };
      gamingModule =
        { pkgs, ... }:
        {
          meta.gaming.enable = true;
          home-manager.users.ivand = {
            wayland.windowManager.sway = {
              config = {
                input = {
                  "type:touchpad" = {
                    events = "disabled";
                  };
                };
                assigns = {
                  "2" = [ { app_id = "^firefox$"; } ];
                  "3" = [
                    { class = "^dota2$"; }
                    { class = "^cs2$"; }
                  ];
                  "9" = [ { class = "^steam$"; } ];
                };
                keybindings = pkgs.lib.mkOptionDefault {
                  "Mod4+o" = "exec ${pkgs.which-key}/bin/which-key";
                };
              };
            };
          };
          systemd.user = {
            timers = {
              steam-desktop-entries = {
                wantedBy = [ "timers.target" ];
                timerConfig = {
                  OnCalendar = "*-*-* 10:00:00";
                  Persistent = true;
                };
              };
            };
            services.steam-desktop-entries =
              let
                exe =
                  pkgs.writers.writePython3 "steam-desktop-entries"
                    {
                      libraries = with pkgs.python3Packages; [ vdf ];
                    }
                    # py
                    ''
                      import glob
                      import os
                      import vdf
                      steamapps = os.path.expanduser("~/.local/share/Steam/steamapps")


                      def save_desktop_entry(appid, name):
                          appid = str(appid).strip()
                          name = (name or "").strip()
                          path = os.path.expanduser(
                              f"~/.local/share/applications/game-{appid}.desktop"
                          )
                          with open(path, "w", encoding="utf-8") as f:
                              f.write(f"""[Desktop Entry]
                      Exec=steam -silent steam://launch/{appid}/dialog
                      Icon=steam_icon_{appid}
                      Name={name}
                      Terminal=false
                      Type=Application
                      Version=1.5
                      """)


                      for path in sorted(glob.glob(os.path.join(steamapps, "appmanifest_*.acf"))):
                          with open(path, "r", encoding="utf-8", errors="replace") as f:
                              data = vdf.load(f)
                          st = data.get("AppState", {})
                          appid = st.get("appid")
                          name = st.get("name")
                          save_desktop_entry(appid, name)
                    '';
              in
              {
                description = "Generate desktop files for all installed games";
                script = ''
                  ${exe}
                '';
              };
          };
          environment.systemPackages = with pkgs; [ radeontop ];
        };
      vpsadminosModule =
        {
          lib,
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
          meta.shells.enable = true;
          meta.dnscrypt.enable = true;
          meta.mail.enable = true;
          meta.bingwp.enable = true;
          meta.wireguard = {
            enable = true;
            address = "10.0.0.1/24";
            inherit peers;
          };
          services.nginx.enable = true;
          services.postgresql.enable = true;
          networking = {
            inherit hosts;
            nftables = {
              enable = true;
            };
            firewall = {
              interfaces = {
                wg0 = {
                  allowedTCPPorts = mkForce [
                    22 # ssh
                    53 # dns
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
                25 # smtp
                465 # smtps
                80 # http
                443 # https
              ];
              allowedUDPPorts = mkForce [
                51820 # wireguard
              ];
            };
          };
          environment.enableAllTerminfo = true;
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
              settings = {
                PasswordAuthentication = false;
                KbdInteractiveAuthentication = false;
                AuthenticationMethods = "publickey";
                AllowGroups = [ "ssh" ];
                ListenAddress = "10.0.0.1";
                PermitRootLogin = "no";
              };
            };
          };
        };
      mail =
        {
          config,
          lib,
          pkgs,
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
                loginAccounts = {
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
                roundcube = {
                  enable = true;
                  package = pkgs.roundcube.withPlugins (plugins: [ plugins.persistent_login ]);
                  plugins = [
                    "persistent_login"
                  ];
                  hostName = "mail.idimitrov.dev";
                  extraConfig = ''
                    $config['imap_host'] = 'ssl://${config.mailserver.fqdn}:993';
                    $config['smtp_host'] = "ssl://${config.mailserver.fqdn}";
                    $config['smtp_user'] = "%u";
                    $config['smtp_pass'] = "%p";
                  '';
                };
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
              defaults = {
                email = "security@idimitrov.dev";
                webroot = "/var/lib/acme/acme-challenge";
              };
              certs = {
                "idimitrov.dev" = {
                  extraDomainNames = [
                    "www.idimitrov.dev"
                    "metronome.idimitrov.dev"
                    "grafana.idimitrov.dev"
                    "pic.idimitrov.dev"
                    "mail.idimitrov.dev"
                  ];
                };
              };
            };
          };
          services.nginx =
            let
              tls = {
                enableACME = false;
                forceSSL = true;
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
              } $uri $uri/ =404";
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
                  listenAddresses = [
                    "10.0.0.1"
                  ];
                  locations."/" = {
                    proxyPass = "http://127.0.0.1:34321";
                    proxyWebsockets = true;
                  };
                };
                "mail.idimitrov.dev" = tls // {
                  listenAddresses = [
                    "10.0.0.1"
                  ];
                };
              };
            };
          services = {
            postgresql.ensureUsers = [
              {
                name = "root";
                ensureClauses = {
                  superuser = true;
                  createrole = true;
                  createdb = true;
                };
              }
            ];
          };
        };
    };
}
