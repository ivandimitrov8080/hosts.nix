{ inputs, ... }:
let
  system = "x86_64-linux";
  nixosModules =
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
        "ATHENS-HAWKS" = { };
        "3G" = {
          psk = "bumshakalaka";
        };
        "LeevinGuest" = {
          psk = "L33v1nGhF4ro";
        };
        "FaroStation" = {
          psk = "riaformosa";
        };
      };
    in
    {
      default =
        { pkgs, ... }:
        {
          imports = with inputs; [
            configuration.nixosModules.default
            hosts.nixosModule
          ];
          nix.registry = {
            self.flake = inputs.self;
            nixpkgs.flake = inputs.nixpkgs;
            p.flake = inputs.nixpkgs;
          };
          nixpkgs = {
            config = {
              allowUnfree = false;
            };
            overlays = [
              inputs.configuration.overlays.default
              inputs.self.overlays.default
            ];
          };
          system.stateVersion = "25.05";
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
                programs = {
                  ssh.matchBlocks = {
                    vpsfree-ivand = {
                      hostname = "idimitrov.dev";
                      user = "ivand";
                    };
                    vpsfree-root = {
                      hostname = "idimitrov.dev";
                      user = "root";
                    };
                    stara-root = {
                      hostname = "stara.idimitrov.dev";
                      user = "root";
                    };
                    git = {
                      hostname = "idimitrov.dev";
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
                      focus = ''
                        # Focus agent

                        You are a software developer assistant focused on doing one thing and doing it well.
                        When dealing with problems focus only on the immediate problem and not on any prerequisites or
                        side-effects. You let the programmer deal with that.

                        ## Guidelines
                        - If the task requires editing
                            a) Focus on making small changes in already existing files
                            b) If anything bigger is required let the programmer know.
                        - If the task does not require editing
                            a) Focus on providing very direct explanations without any digressions.
                      '';
                    };
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
                  nushell.enable = true;
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
                  mpv.scripts = with pkgs.myMpvScripts; [
                    dir-player
                  ];
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
            deadnix
            gimp
            grim
            just
            kotatogram-desktop
            libnotify
            libreoffice-qt
            mupdf
            nvim.default
            pwvucontrol
            python3
            screenshot
            slurp
            statix
            telegram-desktop
            transmission_4
            volume
            wl-clipboard
            xin
          ];
          users = {
            users = {
              ivand = {
                isNormalUser = true;
                createHome = true;
                shell = pkgs.zsh;
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
      nova =
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
            peers = peers;
            address = "10.0.0.2/24";
          };
          networking.hostName = "nova";
          networking = {
            hosts = {
              "10.0.0.1" = [
                "ai.idimitrov.dev"
                "src.idimitrov.dev"
                "idimitrov.dev"
                "mail.idimitrov.dev"
              ];
              "10.0.0.4" = [
                "stara.idimitrov.dev"
              ];
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
          meta.swhkd.enable = true;
          meta.swhkd.graphical = true;
          meta.swhkd.keybindings = {
            "XF86AudioMute" = "volume sink toggle";
            "Shift + XF86AudioMute" = "volume source toggle";
            "XF86AudioLowerVolume" = "volume sink down";
            "Shift + XF86AudioLowerVolume" = "volume source down";
            "XF86AudioRaiseVolume" = "volume sink up";
            "Shift + XF86AudioRaiseVolume" = "volume source up";
            "XF86MonBrightnessUp" = "brightnessctl set 10%+";
            "XF86MonBrightnessDown" = "brightnessctl set 10%-";
            "alt + shift + l" = "swaylock";
            "super + p" = "rofi -show drun";
            "super + shift + s" = "screenshot screen";
            "super + shift + a" = "screenshot area";
            "super + shift + w" = "screenshot window";
            "end" = "rofi -show calc";
          };
        };
      stara = _: {
        programs = {
          git.enable = true;
          gtklock.enable = true;
          zoxide.enable = true;
          zsh.enable = true;
          nix-ld.enable = true;
        };
        networking = {
          hosts = {
            "10.0.0.1" = [
              "ai.idimitrov.dev"
              "src.idimitrov.dev"
              "idimitrov.dev"
              "mail.idimitrov.dev"
            ];
            "10.0.0.4" = [
              "stara.idimitrov.dev"
            ];
          };
          wireless = {
            enable = true;
            networks = wirelessNetworks;
          };
        };
        boot.loader.grub.enable = true;
        meta.shells.enable = true;
        meta.swayland.enable = true;
        meta.wireguard = {
          enable = true;
          peers = peers;
          address = "10.0.0.4/24";
        };
        networking.hostName = "stara";
        services = {
          openssh = {
            enable = true;
            settings = {
              PasswordAuthentication = false;
              PermitRootLogin = "yes";
            };
          };
          dbus.enable = true;
        };
        users.users.ivand.openssh.authorizedKeys.keys = [
          ''
            ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICcLkzuCoBEg+wq/H+hkrv6pLJ8J5BejaNJVNnymlnlo ivan@idimitrov.dev
          ''
        ];
        users.users.root.openssh.authorizedKeys.keys = [
          ''
            ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICcLkzuCoBEg+wq/H+hkrv6pLJ8J5BejaNJVNnymlnlo ivan@idimitrov.dev
          ''
        ];
        networking.firewall = {
          enable = true;
          interfaces = {
            wg0 = {
              allowedTCPPorts = [
                22
                53
                993
                80 # http
                443 # https
                8080 # open-webui
                11434 # ollama
              ];
              allowedUDPPorts = [
                80
                443
                51820 # wireguard
              ];
            };
          };
          allowedTCPPorts = [
            8080 # open-webui
          ];
        };
        meta.ai.enable = true;
        services = {
          open-webui = {
            enable = true;
            host = "0.0.0.0";
          };
          monero = {
            enable = true;
            dataDir = "/data/var/lib/monero";
          };
        };
      };
      vpsadminosModule =
        { pkgs, lib, ... }:
        let
          inherit (lib) mkForce;
        in
        {
          nixpkgs.hostPlatform = "x86_64-linux";
          imports = with inputs; [
            vpsadminos.nixosConfigurations.containerUnstable
            simple-nixos-mailserver.nixosModule
            webshite.nixosModules.default
          ];
          networking.hostName = "vpsfree";
          meta.shells.enable = true;
          meta.dnscrypt.enable = true;
          meta.mail.enable = true;
          meta.bingwp.enable = true;
          meta.wireguard = {
            enable = true;
            address = "10.0.0.1/24";
            peers = peers;
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
                    22
                    53
                    993
                    9418 # gitDaemon
                  ];
                  allowedUDPPorts = mkForce [
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
                25
                465
                80
                443
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
                ];
                openssh.authorizedKeys.keys = [
                  ''
                    ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICcLkzuCoBEg+wq/H+hkrv6pLJ8J5BejaNJVNnymlnlo ivan@idimitrov.dev
                  ''
                ];
              };
              git = {
                shell = pkgs.bash;
                openssh.authorizedKeys.keys = [
                  ''
                    ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICcLkzuCoBEg+wq/H+hkrv6pLJ8J5BejaNJVNnymlnlo ivan@idimitrov.dev
                  ''
                ];
              };
            };
          };

          programs.git.enable = true;
          services = {
            gitDaemon = {
              enable = true;
              repositories = [
                "/srv/git"
              ];
              basePath = "/srv/git";
              exportAll = true;
              listenAddress = "127.0.0.1";
            };
            openssh = {
              enable = true;
              settings = {
                PasswordAuthentication = false;
                PermitRootLogin = "prohibit-password";
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
                certificateScheme = "acme-nginx";
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
                nginx.virtualHosts =
                  let
                    restrictToVpn = ''
                      allow 127.0.0.1/32;
                      allow 10.0.0.2/32;
                      allow 10.0.0.3/32;
                      allow 10.0.0.4/32;
                      allow 10.0.0.5/32;
                      deny all;
                    '';
                  in
                  {
                    "mail.idimitrov.dev" = {
                      extraConfig = restrictToVpn;
                    };
                  };
                postgresql.enable = true;
              };
              security = {
                acme = {
                  acceptTerms = true;
                  defaults.email = "security@idimitrov.dev";
                };
              };
            }
          );
        };
      nginx = {
        services.nginx.virtualHosts = {
          "src.idimitrov.dev" = {
            enableACME = true;
            forceSSL = true;
            locations."/" = {
              proxyPass = "http://127.0.0.1:9418";
              extraConfig = ''
                allow 10.0.0.0/8;
                allow 192.168.0.0/8;
                deny all;
                proxy_http_version 1.1;
                proxy_set_header Upgrade $http_upgrade;
                proxy_set_header Connection "upgrade";
              '';
            };
          };
          "pic.idimitrov.dev" = {
            enableACME = true;
            forceSSL = true;
            locations."/" = {
              root = "/var/pic";
              extraConfig = ''
                autoindex on;
              '';
            };
          };
          "ai.idimitrov.dev" = {
            enableACME = true;
            forceSSL = true;
            locations."/" = {
              proxyPass = "http://10.0.0.4:8080";
              extraConfig = ''
                allow 10.0.0.0/8;
                allow 192.168.0.0/8;
                deny all;
                proxy_http_version 1.1;
                proxy_set_header Upgrade $http_upgrade;
                proxy_set_header Connection "upgrade";
              '';
            };
          };
        };
        services.postgresql.ensureUsers = [
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
  hardwareConfigurations = import ../constants;
  configWithModules =
    {
      hardware ? {
        nixpkgs.hostPlatform = system;
      },
      modules,
    }:
    inputs.nixpkgs.lib.nixosSystem {
      modules = [
        hardware
      ]
      ++ modules;
    };
  novaConfig =
    mods:
    configWithModules {
      hardware = hardwareConfigurations.nova;
      modules =
        (with nixosModules; [
          default
          rest
          nova
          swhkd
        ])
        ++ mods;
    };
  staraConfig =
    mods:
    configWithModules {
      hardware = hardwareConfigurations.stara;
      modules =
        (with nixosModules; [
          default
          rest
          stara
        ])
        ++ mods;
    };
  vpsConfig =
    mods:
    configWithModules {
      modules =
        (with nixosModules; [
          vpsadminosModule
          default
          mail
          nginx
        ])
        ++ mods;
    };
  swhkd =
    {
      lib,
      config,
      pkgs,
      ...
    }:
    let
      inherit (lib)
        mkIf
        mkEnableOption
        mkOption
        literalExpression
        types
        ;
      cfg = config.meta.swhkd;
      keybindingsStr = lib.concatStringsSep "\n" (
        lib.mapAttrsToList (
          hotkey: command:
          lib.optionalString (command != null) ''
            ${hotkey}
              ${command}
          ''
        ) cfg.keybindings
      );
      configFileContent = lib.concatStringsSep "\n" [
        keybindingsStr
        cfg.extraConfig
      ];
    in
    {
      options.meta.swhkd = {
        enable = mkEnableOption "enable swhkd config";
        graphical = mkEnableOption "whether commands to run require graphical session";
        extraConfig = mkOption {
          default = "";
          type = types.lines;
          description = "Additional configuration to add.";
          example = literalExpression ''
            super + {_,shift +} {1-9,0}
              i3-msg {workspace,move container to workspace} {1-10}
          '';
        };
        keybindings = mkOption {
          type = types.attrsOf (
            types.nullOr (
              types.oneOf [
                types.str
                types.path
              ]
            )
          );
          default = { };
          description = "An attribute set that assigns hotkeys to commands.";
          example = literalExpression ''
            {
              "super + shift + {r,c}" = "i3-msg {restart,reload}";
              "super + {s,w}"         = "i3-msg {stacking,tabbed}";
              "super + F1"            = pkgs.writeShellScript "script" "echo $USER";
            }
          '';
        };
      };
      config = mkIf cfg.enable {
        environment.systemPackages = [ pkgs.swhkd ];
        security.wrappers.swhkd = {
          source = "${pkgs.swhkd}/bin/swhkd";
          setuid = true;
          owner = "root";
          group = "root";
        };
        systemd.user.services = {
          swhks = {
            wantedBy = [
              (if cfg.graphical then "graphical-session.target" else "default.target")
            ];
            requiredBy = [ "swhkd.service" ];
            serviceConfig = {
              Type = "forking";
              ExecStart = "${pkgs.swhkd}/bin/swhks";
            };
          };
          swhkd = {
            wantedBy = [
              (if cfg.graphical then "graphical-session.target" else "default.target")
            ];
            requires = [ "swhks.service" ];
            serviceConfig = {
              ExecStart = "/run/wrappers/bin/swhkd";
            };
          };
        };
        environment.etc."swhkd/swhkdrc".text = configFileContent;
      };
    };
in
{
  nova = novaConfig [
    (
      { pkgs, lib, ... }:
      let
        modprobedDb = ./modprobed.db;
        kernel = pkgs.linuxPackages-libre.kernel;
        generatedConfig = pkgs.stdenv.mkDerivation {
          name = "linux-nova";
          src = kernel.src;
          nativeBuildInputs = with pkgs; [
            pkg-config
            flex
            bison
            perl
          ];

          phases = [
            "unpackPhase"
            "configurePhase"
            "installPhase"
          ];

          configurePhase = ''
            yes "" | make LSMOD=${modprobedDb} localmodconfig
          '';

          installPhase = ''
            runHook preInstall
            mkdir -p $out
            cp .config $out/config
            runHook postInstall
          '';
        };
        kernelPackages = pkgs.linuxPackagesFor (
          kernel.override {
            configfile = "${generatedConfig}/config";
          }
        );
      in
      {
        environment.systemPackages = with pkgs; [ modprobed-db ];
        boot.kernelPackages = kernelPackages;
        boot.extraModprobeConfig = ''
          # unused network protocols
          install dccp /bin/false
          install sctp /bin/false
          install rds /bin/false
          install tipc /bin/false
          install n-hdlc /bin/false
          install ax25 /bin/false
          install netrom /bin/false
          install x25 /bin/false
          install rose /bin/false
          install decnet /bin/false
          install econet /bin/false
          install af_802154 /bin/false
          install ipx /bin/false
          install appletalk /bin/false
          install psnap /bin/false
          install p8023 /bin/false
          install p8022 /bin/false
          install can /bin/false
          install atm /bin/false

          # firewire and thunderbolt
          install firewire-core /bin/false
          install firewire_core /bin/false
          install firewire-ohci /bin/false
          install firewire_ohci /bin/false
          install firewire_sbp2 /bin/false
          install firewire-sbp2 /bin/false
          install firewire-net /bin/false
          install thunderbolt /bin/false
          install ohci1394 /bin/false
          install sbp2 /bin/false
          install dv1394 /bin/false
          install raw1394 /bin/false
          install video1394 /bin/false

          # unused filesystems
          install cramfs /bin/false
          install freevxfs /bin/false
          install jffs2 /bin/false
          install hfs /bin/false
          install hfsplus /bin/false
          install squashfs /bin/false
          install udf /bin/false
          install cifs /bin/false
          install nfs /bin/false
          install nfsv3 /bin/false
          install nfsv4 /bin/false
          install ksmbd /bin/false
          install gfs2 /bin/false
          install reiserfs /bin/false
          install kafs /bin/false
          install orangefs /bin/false
          install 9p /bin/false
          install adfs /bin/false
          install affs /bin/false
          install afs /bin/false
          install befs /bin/false
          install ceph /bin/false
          install coda /bin/false
          install ecryptfs /bin/false
          install jfs /bin/false
          install minix /bin/false
          install netfs /bin/false
          install nilfs2 /bin/false
          install ocfs2 /bin/false
          install romfs /bin/false
          install ubifs /bin/false
          install zonefs /bin/false
          install sysv /bin/false
          install ufs /bin/false

          # disable vivid
          install vivid /bin/false

          # disable GNSS
          install gnss /bin/false
          install gnss-mtk /bin/false
          install gnss-serial /bin/false
          install gnss-sirf /bin/false
          install gnss-usb /bin/false
          install gnss-ubx /bin/false

          # https://en.wikipedia.org/wiki/Bluetooth#History_of_security_concerns
          install bluetooth /bin/false
          install btusb /bin/false

          # blacklist ath_pci
          blacklist ath_pci

          # blacklist cdrom
          blacklist cdrom
          blacklist sr_mod

          # blacklist framebuffer drivers
          # source, ubuntu: https://git.launchpad.net/ubuntu/+source/kmod/tree/debian/modprobe.d/blacklist-framebuffer.conf
          blacklist cyber2000fb
          blacklist cyblafb
          blacklist gx1fb
          blacklist hgafb
          blacklist kyrofb
          blacklist lxfb
          blacklist matroxfb_base
          blacklist neofb
          blacklist nvidiafb
          blacklist pm2fb
          blacklist s1d13xxxfb
          blacklist sisfb
          blacklist tdfxfb
          blacklist vesafb
          blacklist vfb
          blacklist vt8623fb
          blacklist udlfb
        '';
      }
    )
  ];
  gaming = novaConfig [
    (
      { pkgs, ... }:
      let
        desktopItems = [
          (pkgs.makeDesktopItem {
            name = "dota";
            desktopName = "DotA 2";
            exec = "${pkgs.steam}/bin/steam steam://launch/570/dialog";
            terminal = false;
            icon = "${pkgs.faenza}/Delft/apps/96/dota2.svg";
          })
          (pkgs.makeDesktopItem {
            name = "cs2";
            desktopName = "Counter Strike 2";
            exec = "${pkgs.steam}/bin/steam steam://launch/730/dialog";
            terminal = false;
            icon = "${pkgs.faenza}/Delft/apps/96/csgo.svg";
          })
        ];
      in
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
                "3" = [
                  { class = "^dota2$"; }
                  { class = "^cs2$"; }
                ];
                "9" = [ { class = "^steam$"; } ];
              };
            };
          };
        };
        environment.systemPackages = with pkgs; [ radeontop ] ++ desktopItems;
      }
    )
  ];
  ai = novaConfig [ { meta.ai.enable = true; } ];
  music = novaConfig [ { meta.music.enable = true; } ];
  stara = staraConfig [ ];
  vps = vpsConfig [ { webshite.enable = true; } ];
}
