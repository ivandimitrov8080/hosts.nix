{ pkgs, lib, ... }:
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
    ];
  };
in
pkgs.writeShellScriptBin "which-key" ''
  exec ${pkgs.wlr-which-key}/bin/wlr-which-key ${pkgs.writeText "config.yaml" menu}
''
