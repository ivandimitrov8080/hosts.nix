{ inputs }:
let
  intel = "x86_64-linux";
  nixosModules = inputs.self.nixosModules.default;
  hardwareConfigurations = import ../constants;
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
      ])
      ++ [ hardwareConfigurations.nova ];
  };
}
