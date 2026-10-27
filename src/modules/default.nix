{ inputs, overlays }:
let
  system = "x86_64-linux";
  dns = "10.0.0.1";
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
      PublicKey = "IDe1MPtS46c2iNcE+VrOSUpOVGMXjqFl+XV5Z5U+DDI=";
      AllowedIPs = [ "10.0.0.5/32" ];
    }
  ];
  # TODO: make something similar for vps where it can also send dns traffic back to wireguard peers
  blockDnsExceptDnscrypt = ''
    table inet filter {
      chain output {
        type filter hook output priority 0; policy accept;

        meta oifname "lo" accept

        # Allow dns to server
        ip daddr ${dns} udp dport 53 accept
        ip daddr ${dns} tcp dport 53 accept

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
        hostPlatform = system;
        overlays = [
          overlays.default
          overlays.config
          overlays.emacs
        ];
      };
      system.stateVersion = pkgs.lib.trivial.release;
      users.defaultUserShell = pkgs.nushell;
    };
  wg = { lib, ... }: {
    meta.wireguard = {
      inherit peers dns;
    };
    networking.nameservers = [ dns ];
    networking.nftables = {
      enable = true;
      ruleset = lib.mkDefault blockDnsExceptDnscrypt;
    };
    services.resolved = {
      settings = {
        Resolve = {
          FallbackDNS = [ dns ];
        };
      };
    };
  };
  minimal =
    { pkgs, ... }:
    {
      imports = with inputs; [
        home-manager.nixosModules.default
      ];
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
      security.sudo-rs.enable = true;
      environment = {
        systemPackages = with pkgs; [
          ffmpeg
          gcc
        ];
        wordlist.enable = true;
        variables = {
          OPTIONS_JSON_NIXOS = "${pkgs.docs-nixos}/share/doc/nixos/options.json";
          OPTIONS_JSON_HOME_MANAGER = "${pkgs.docs-hm}/share/doc/home-manager/options.json";
          SUDO_ASKPASS = "${pkgs.askpass}/bin/askpass";
          NIX_SUDOOPTS = "-A";
        };
        etc."libinput/local-overrides.quirks".text = ''
          [KMonad Virtual Keyboard]
          MatchName=kmonad-laptop
          AttrKeyboardIntegration=internal
        '';
      };
      fonts = {
        fontDir.enable = true;
        packages = with pkgs; [
          nerd-fonts.fira-code
          nerd-fonts.symbols-only
          noto-fonts
          noto-fonts-color-emoji
          noto-fonts-lgc-plus
          emacs-all-the-icons-fonts
          inter
        ];
      };
      programs = {
        git.enable = true;
        zoxide.enable = true;
        zsh.enable = true;
        nix-ld.enable = true;
        nix-ld.libraries = with pkgs; [
          alsa-lib
          at-spi2-atk
          cairo
          cups
          dbus
          expat
          glib
          gtk3
          libgbm
          libx11
          libxcb
          libxcomposite
          libxdamage
          libxext
          libxfixes
          libxkbcommon
          libxrandr
          libxrender
          libxtst
          libxi
          freetype
          vulkan-loader
          libGL
          nspr
          nss
          pango
        ];
        ladybird.enable = true;
      };
      services = {
        pipewire.enable = true;
        dbus.enable = true;
        locate.enable = true;
        resolved.enable = true;
        guix.enable = true;
        libretranslate.enable = true;
        transmission.enable = true;
        kmonad = {
          enable = true;
          keyboards.laptop = {
            device = "/dev/input/by-path/platform-i8042-serio-0-event-kbd";
            defcfg.enable = true;
            config = ''
              (defsrc
                esc  f1   f2   f3   f4   f5   f6   f7   f8   f9   f10  f11  f12  ssrq pause ins del
                grv  1    2    3    4    5    6    7    8    9    0    -    =    bspc
                tab  q    w    e    r    t    y    u    i    o    p    [    ]
                caps a    s    d    f    g    h    j    k    l    ;    '    \    ret
                lsft z    x    c    v    b    n    m    ,    .    /    rsft       pgup up   pgdn
                lctl lmet lalt           spc                 ralt rctl            left down rght
              )
              (deflayer base
                esc  f1   f2   f3   f4   f5   f6   f7   f8   f9   f10  f11  f12  ssrq pause ins del
                grv  1    2    3    4    5    6    7    8    9    0    -    =    bspc
                tab  q    w    e    r    t    y    u    i    o    p    [    ]
                caps a    s    d    f    g    h    j    k    l    ;    '    \    ret
                lsft z    x    c    v    b    n    m    ,    .    /    rsft       left up   right
                lctl lmet lalt           spc                 ralt rctl            left down rght
              )
            '';
          };
        };
      };
      hardware.uinput.enable = true;
      meta = {
        graphicalBoot.enable = true;
        shells.enable = true;
        swayland.enable = true;
      };
    };
}
