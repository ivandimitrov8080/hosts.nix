{ inputs }:
let
  intel = "x86_64-linux";
  arm = "aarch64-linux";
  overlays = [
    inputs.self.overlays.default
    inputs.self.overlays.config
  ];
  pkgs = import inputs.nixpkgs {
    inherit overlays;
    system = intel;
  };
  armPkgs = import inputs.nixpkgs {
    inherit overlays;
    system = arm;
  };
  nixosModules = inputs.self.nixosModules.default;
  hardwareConfigurations = import ../constants;
  metal = inputs.nixpkgs.lib.nixosSystem {
    modules = with nixosModules; [
      default
      minimal
    ];
  };
in
rec {
  iso = inputs.nixpkgs.lib.nixosSystem {
    modules = with nixosModules; [
      default
      minimal
      nixosModules.iso
    ];
  };
  vps = inputs.nixpkgs.lib.nixosSystem {
    modules = with nixosModules; [
      vpsadminosModule
      default
      wg
      mail
      nginx
      {
        nixpkgs.hostPlatform = "x86_64-linux";
        imports = with inputs; [
          vpsadminos.nixosConfigurations.containerUnstable
        ];
        _module.args.system = intel;
      }
    ];
  };
  nova = metal.extendModules {
    modules =
      (with nixosModules; [
        wg
        rest
        (
          { pkgs, ... }:
          {
            home-manager.users.ivand = {
              wayland.windowManager.sway = {
                config = {
                  keybindings = pkgs.lib.mkOptionDefault {
                    "Mod4+o" = "exec ${pkgs.which-key}/bin/which-key";
                  };
                };
              };
            };
          }
        )
      ])
      ++ [ hardwareConfigurations.nova ];
  };
  gaming = nova.extendModules {
    modules = with nixosModules; [
      (
        { pkgs, lib, ... }:
        {
          meta.gaming.enable = true;
          nixpkgs.config = {
            allowUnfree = lib.mkForce false;
          };
          nixpkgs.config.allowUnfreePredicate =
            pkg:
            builtins.elem (lib.getName pkg) [
              "steam"
              "steam-original"
              "steam-unwrapped"
              "steam-run"
              "discord"
            ];
          systemd = {
            network.networks.wg0 = {
              routingPolicyRules = import ./gaming/steam-route-rules.nix;
            };
          };
          environment.systemPackages = with pkgs; [ radeontop ];
        }
      )
    ];
  };
  mobile = import inputs.mobile-nixos {
    device = "oneplus-enchilada";
    pkgs = armPkgs;
    configuration =
      { lib, ... }:
      {
        mobile.boot.stage-1.networking.enable = true;
        nixpkgs.config.allowUnfreePredicate =
          pkg:
          builtins.elem (lib.getName pkg) [
            "oneplus-sdm845-firmware"
            "oneplus-sdm845-firmware-zstd"
          ];
        system.stateVersion = lib.trivial.release;
        imports = with inputs; [
          home-manager.nixosModules.default
          configuration.nixosModules.default
          nixosModules.wg
        ];
        nix.registry = {
          self.flake = inputs.self;
          nixpkgs.flake = inputs.nixpkgs;
          p.flake = inputs.nixpkgs;
        };
        hardware = {
          bluetooth.enable = true;
        };
        time.timeZone = "Europe/Prague";
        users = {
          users.user = {
            isNormalUser = true;
            password = "1234";
            extraGroups = [
              "ssh"
              "adm"
              "audio"
              "bluetooth"
              "dialout"
              "input"
              "mlocate"
              "render"
              "video"
              "wheel"
            ];
            openssh.authorizedKeys.keys = [
              "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICcLkzuCoBEg+wq/H+hkrv6pLJ8J5BejaNJVNnymlnlo ivan@idimitrov.dev"
            ];
          };
          groups = {
            ssh = { };
          };
        };
        networking = {
          networkmanager.enable = true;
          wireless.enable = true;
          useNetworkd = true;
          firewall = {
            enable = true;
            checkReversePath = false;
            allowedTCPPorts = [ 22 ];
            allowedUDPPorts = [ 22 ];
          };
        };
        services = {
          openssh.enable = true;
          xserver.desktopManager.phosh = {
            enable = true;
            user = "user";
            group = "users";
            phocConfig.xwayland = "immediate";
          };
          pipewire.enable = true;
          locate.enable = true;
        };
        programs = {
          firefox =
            let
              mcf = armPkgs.mobile-config-firefox;
              autoconfigPatched =
                builtins.replaceStrings
                  [ "/usr/lib/mobile-config-firefox/" ]
                  [ "${mcf}/usr/lib/mobile-config-firefox/" ]
                  (builtins.readFile "${mcf}/usr/lib/firefox/mobile-config-autoconfig.js");
            in
            {
              enable = true;
              policies = builtins.fromJSON (
                builtins.readFile "${mcf}/usr/lib/firefox/distribution/policies.json"
              );
              autoConfigFiles = [
                "${mcf}/usr/lib/firefox/defaults/pref/mobile-config-prefs.js"
              ];
              autoConfig = autoconfigPatched;
              nativeMessagingHosts.packages = with armPkgs; [ browserpass ];
            };
          dconf = {
            enable = true;
            profiles = {
              user.databases = [
                {
                  settings = {
                    "mobi/phosh/osk" = {
                      completion-mode = [ "manual" ];
                    };
                    "mobi/phosh/osk/terminal" = {
                      shortcuts = [
                        "<ctrl>"
                        "<ctrl>c"
                        "<alt>"
                        "Left"
                        "Up"
                        "Down"
                        "Right"
                      ];
                    };
                  };
                }
              ];
            };
          };
        };
        environment = {
          systemPackages = with armPkgs; [
            simplex-chat-desktop
            nushell
            pwvucontrol
            gapless
            (makeDesktopItem {
              name = "telegram";
              desktopName = "Telegram";
              exec = "env ${telegram-desktop}/bin/Telegram -- %U";
              terminal = false;
              icon = "${telegram-desktop}/share/icons/hicolor/128x128/apps/org.telegram.desktop.png";
            })
          ];
        };
        meta = {
          wireguard = {
            enable = true;
            address = "10.0.0.6/24";
          };
          shells.enable = true;
        };
        home-manager = {
          backupFileExtension = "bak";
          useUserPackages = true;
          useGlobalPkgs = true;
          users.user =
            { ... }:
            {
              imports = [ inputs.configuration.homeManagerModules.default ];
              xdg.enable = true;
              home = {
                username = "user";
                homeDirectory = "/home/user";
              };
              programs = {
                bash.enable = true;
                bat.enable = true;
                bottom.enable = true;
                browserpass.enable = true;
                eza.enable = true;
                fd.enable = true;
                fzf.enable = true;
                git.enable = true;
                gpg.enable = true;
                kitty.enable = true;
                mpv.enable = true;
                nushell.enable = true;
                password-store.enable = true;
                ssh.enable = true;
                starship.enable = true;
                taskwarrior.enable = true;
                tealdeer.enable = true;
                tmux.enable = true;
                yazi.enable = true;
                zoxide.enable = true;
              };
              services = {
                gpg-agent.enable = true;
              };
            };
        };
      };
  };
}
