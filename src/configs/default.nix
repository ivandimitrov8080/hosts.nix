{ inputs, ... }:
let
  system = "x86_64-linux";
  pkgs = import inputs.nixpkgs { inherit system; };
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
      hosts = {
        "10.0.0.1" = [
          "mail.idimitrov.dev"
        ];
      };
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
            config = {
              allowUnfree = false;
            };
            overlays = [
              inputs.self.overlays.default
              inputs.self.overlays.config
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
                  direnv = {
                    enable = true;
                    enableZshIntegration = true;
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
                  # mpv.scripts = with pkgs.myMpvScripts; [
                  #   dir-player
                  # ];
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
                  ollama.acceleration = "rocm";
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
            hosts = hosts;
            nftables.enable = true;
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
            hosts = hosts;
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
                ];
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
                x509.useACMEHost = "idimitrov.dev";
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
                nginx.virtualHosts = {
                  "mail.idimitrov.dev" = {
                    listenAddresses = [
                      "10.0.0.1"
                    ];
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
          "metronome.idimitrov.dev" = {
            enableACME = true;
            forceSSL = true;
            locations."/" = {
              root = inputs.metronome.packages.${system}.default;

              extraConfig = ''
                autoindex on;
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
in
rec {
  nova = inputs.nixpkgs.lib.nixosSystem {
    modules =
      (with nixosModules; [
        default
        rest
        nova-module
      ])
      ++ [ hardwareConfigurations.nova ];
  };
  gaming = nova.extendModules {
    modules = [
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
                  "2" = [ { app_id = "^firefox$"; } ];
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
  };
  ai = nova.extendModules { modules = [ ({ meta.ai.enable = true; }) ]; };
  music = nova.extendModules {
    modules = [
      {
        meta.music.enable = true;
        boot.kernelPackages = pkgs.linuxPackages_zen;
      }
    ];
  };
  vps = inputs.nixpkgs.lib.nixosSystem {
    modules = (
      with nixosModules;
      [
        vpsadminosModule
        default
        mail
        nginx
        {
          webshite.enable = true;
          _module.args.system = system;
        }
      ]
    );
  };
}
