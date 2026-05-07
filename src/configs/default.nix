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
              ''
                ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICcLkzuCoBEg+wq/H+hkrv6pLJ8J5BejaNJVNnymlnlo ivan@idimitrov.dev
              ''
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
        environment = {
          systemPackages = with armPkgs; [
            simplex-chat-desktop
            nushell
            (makeDesktopItem {
              name = "telegram";
              desktopName = "Telegram";
              exec = "env ${telegram-desktop}/bin/Telegram -- %U";
              terminal = false;
              icon = "${telegram-desktop}/share/icons/hicolor/128x128/apps/org.telegram.desktop.png";
            })
            pwvucontrol
          ];
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
                firefox.enable = true;
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
