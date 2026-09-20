{ inputs }:
let
  intel = "x86_64-linux";
  nixosModules = inputs.self.nixosModules.default;
  metal = inputs.nixpkgs.lib.nixosSystem {
    modules = with nixosModules; [
      default
      minimal
    ];
  };
in
{
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
    modules = with nixosModules; [
      wg
      rest
      (
        {
          pkgs,
          ...
        }:
        {
          imports = [
            inputs.disko.nixosModules.disko
          ];
          hardware.firmware = [ pkgs.linux-firmware ];
          boot.initrd = {
            availableKernelModules = [
              "xhci_pci"
              "thunderbolt"
              "nvme"
              "usb_storage"
              "sd_mod"
              "sdhci_pci"
            ];
          };
          disko.devices = {
            disk = {
              main = {
                type = "disk";
                device = "/dev/nvme0n1";
                content = {
                  type = "gpt";
                  partitions = {
                    ESP = {
                      size = "1G";
                      type = "EF00";
                      uuid = "01d82347-b293-46df-a0b8-94e8e4cf4d9e";
                      content = {
                        type = "filesystem";
                        format = "vfat";
                        mountpoint = "/boot";
                        mountOptions = [
                          "uid=0"
                          "gid=0"
                          "fmask=0077"
                          "dmask=0077"
                        ];
                      };
                    };
                    luks = {
                      size = "100%";
                      uuid = "64323137-3864-6162-2d64-3562342d3433";
                      content = {
                        type = "luks";
                        name = "nixos";
                        content = {
                          type = "btrfs";
                          extraArgs = [ "-f" ];
                          mountpoint = "/";
                        };
                      };
                    };
                  };
                };
              };
            };
          };
          home-manager.users.ivand = { lib, config, ... }: {
            wayland.windowManager.sway = {
              config = {
                keybindings = pkgs.lib.mkOptionDefault {
                  "Mod4+o" = "exec ${pkgs.which-key}/bin/which-key";
                };
                startup = [
                  { command = "exec ${lib.getExe config.programs.firefox.package}"; }
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
          };
        }
      )
    ];
  };
}
