{
  pkgs,
  lib,
  rustPlatform,
  fetchFromGitHub,
  ...
}:
let
  menu = lib.generators.toYAML { } {
    anchor = "center";
    menu = [
      {
        key = "r";
        desc = "Restart";
        submenu = [
          {
            key = "r";
            desc = "Reboot";
            cmd = "reboot";
          }
          {
            key = "o";
            desc = "Turn Off";
            cmd = "poweroff";
          }
        ];
      }
      {
        key = "p";
        desc = "Processes";
        submenu = [
          {
            key = "k";
            desc = "Kill";
            cmd = # bash
              ''
                ps -u "$USER" -o pid=,comm=,args= --sort=-pid | awk '{print $1 " " $2}' | rofi -dmenu -i -p "kill -9" | awk '{print $1}' | xargs -r kill -9
              '';
          }
        ];
      }
      {
        key = "v";
        desc = "Volume";
        submenu = [
          {
            key = "m";
            desc = "Microphone";
            submenu = [
              {
                key = "s";
                desc = "Set volume";
                cmd = # bash
                  ''
                    rofi -dmenu -i -p "Volume %" | awk 'NF{printf "%d\n",$1}' | xargs -r -I{} wpctl set-volume @DEFAULT_AUDIO_SOURCE@ {}%
                  '';
              }
              {
                key = "t";
                desc = "Toggle Mute";
                cmd = # bash
                  ''
                    wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle
                  '';
              }
            ];
          }
          {
            key = "s";
            desc = "Speakers";
            submenu = [
              {
                key = "s";
                desc = "Set volume";
                cmd = # bash
                  ''
                    rofi -dmenu -i -p "Volume %" | awk 'NF{printf "%d\n",$1}' | xargs -r -I{} wpctl set-volume @DEFAULT_AUDIO_SINK@ {}%
                  '';
              }
              {
                key = "t";
                desc = "Toggle Mute";
                cmd = # bash
                  ''
                    wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle
                  '';
              }
            ];
          }
        ];
      }
      {
        key = "s";
        desc = "Screen";
        submenu = [
          {
            key = "s";
            desc = "Set brightness";
            cmd = # bash
              ''
                rofi -dmenu -i -p "Brightness %" | awk 'NF{printf "%d\n",$1}' | xargs -r -I{} brightnessctl set {}%
              '';
          }
        ];
      }
    ];
  };
  package = rustPlatform.buildRustPackage (_finalAttrs: rec {
    pname = "wlr-which-key";
    version = "1.3.1";

    src = fetchFromGitHub {
      owner = "ivandimitrov8080";
      repo = "wlr-which-key";
      rev = "4e2940aa127e873b74494c617b1365184c149292";
      hash = "sha256-pv8SgNot3eNRxhKtTA77JHxpBBq/crPLRMGKCsWIS4g=";
    };
    nativeBuildInputs = with pkgs; [
      pkg-config
    ];
    buildInputs = with pkgs; [
      cairo
      pango.dev
      libxkbcommon
    ];

    cargoHash = "sha256-v+4/lD00rjJvrQ2NQqFusZc0zQbM9mBG5T9bNioNGKQ=";

    meta = {
      description = "Keymap manager for wlroots-based compositors";
      homepage = "https://github.com/ivandimitrov8080/wlr-which-key";
      license = lib.licenses.gpl3Only;
      mainProgram = pname;
      maintainers = [ ];
    };
  });
in
pkgs.writeShellScriptBin "which-key" ''
  exec ${package}/bin/wlr-which-key ${pkgs.writeText "config.yaml" menu}
''
