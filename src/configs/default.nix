{ inputs }:
let
  intel = "x86_64-linux";
  arm = "aarch64-linux";
  overlays = [
    inputs.self.overlays.default
    inputs.self.overlays.config
  ];
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
          {
            pkgs,
            ...
          }:
          {
            home-manager.users.ivand = (
              { lib, config, ... }: {
                wayland.windowManager.sway = {
                  config = {
                    keybindings = pkgs.lib.mkOptionDefault {
                      "Mod4+o" = "exec ${pkgs.which-key}/bin/which-key";
                    };
                    startup = [
                      { command = "exec ${lib.getExe config.programs.firefox.package}"; }
                      { command = "swaymsg 'workspace 1; ${pkgs.emacs-custom}/bin/emacsclient'"; }
                    ];
                    assigns = {
                      "2" = [ { app_id = "^${config.programs.firefox.package.meta.mainProgram}$"; } ];
                    };
                    input = {
                      "*" = {
                        xkb_layout = "us,bg";
                        xkb_options = "grp:win_space_toggle";
                        xkb_variant = ",phonetic";
                      };
                    };
                  };
                };
              }
            );
          }
        )
      ])
      ++ [ hardwareConfigurations.nova ];
  };
  gaming = nova.extendModules {
    modules = with nixosModules; [
      penetration
      (
        { pkgs, lib, ... }:
        {
          meta.gaming.enable = true;
          meta.penetration.enable = true;
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
              "discord-unwrapped"
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
  htb = nova.extendModules {
    modules = with nixosModules; [
      penetration
      (
        { lib, ... }:
        {
          meta.penetration.enable = true;
          meta.wireguard.enable = lib.mkForce false;
          networking = {
            nftables.ruleset = lib.mkForce "";
          };
        }
      )
    ];
  };
}
