{ inputs }:
let
  system = "x86_64-linux";
  pkgs = import inputs.nixpkgs { inherit system; };
  nixosModules = inputs.self.nixosModules.default;
  hardwareConfigurations = import ../constants;
  allowUnfree =
    { lib, ... }:
    {
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
    };
in
rec {
  nova = inputs.nixpkgs.lib.nixosSystem {
    modules =
      (with nixosModules; [
        default
        rest
      ])
      ++ [ hardwareConfigurations.nova ];
  };
  gaming = nova.extendModules {
    modules = with nixosModules; [
      gamingModule
      penetration
      allowUnfree
      ({
        meta.penetration.enable = true;
      })
    ];
  };
  music = nova.extendModules {
    modules = [
      {
        meta.music.enable = true;
        boot.kernelPackages = pkgs.linuxPackages_zen;
      }
    ];
  };
  vps = inputs.nixpkgs.lib.nixosSystem {
    modules = with nixosModules; [
      vpsadminosModule
      default
      mail
      nginx
      {
        nixpkgs.hostPlatform = "x86_64-linux";
        imports = with inputs; [
          vpsadminos.nixosConfigurations.containerUnstable
        ];
        _module.args.system = system;
      }
    ];
  };
}
