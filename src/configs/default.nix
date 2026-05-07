{ inputs }:
let
  intel = "x86_64-linux";
  arm = "aarch64-linux";
  pkgs = import inputs.nixpkgs { system = intel; };
  armPkgs = import inputs.nixpkgs { system = arm; };
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
        rest
      ])
      ++ [ hardwareConfigurations.nova ];
  };
  gaming = nova.extendModules {
    modules = with nixosModules; [
      gamingModule
      allowUnfree
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
  mobile = import inputs.mobile-nixos {
    device = "oneplus-enchilada";
    pkgs = armPkgs;
    configuration =
      { lib, ... }:
      {
        mobile.boot.stage-1.networking.enable = true;
        system.stateVersion = lib.trivial.release;
        users.users.user = {
          isNormalUser = true;
          password = "1234";
          extraGroups = [
            "wheel"
            "ssh"
          ];
          openssh.authorizedKeys.keys = [
            ''
              ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICcLkzuCoBEg+wq/H+hkrv6pLJ8J5BejaNJVNnymlnlo ivan@idimitrov.dev
            ''
          ];
        };
        services = {
          openssh.enable = true;
          desktopManager.gnome.enable = true;
          displayManager = {
            autoLogin = {
              enable = true;
              user = "user";
            };
            gdm = {
              enable = true;
            };
          };
        };
        networking = {
          networkmanager.enable = false;
          useNetworkd = true;
          firewall = {
            enable = true;
            allowedTCPPorts = [
              22
              80
            ];
            allowedUDPPorts = [
              22
              53
            ];
          };
          wireless = {
            enable = true;
            networks = {
              "John94".psk = "John9401";
            };
          };
        };
      };
  };
}
