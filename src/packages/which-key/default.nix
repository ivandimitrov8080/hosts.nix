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
                ps -u "$USER" -o pid=,comm=,args= --sort=-pid | awk '{print $1 " " $2}' | rofi -dmenu -i -p "KILL -9 (select process)" | awk '{print $1}' | xargs -r kill -9
              '';
          }
        ];
      }
    ];
  };
in
pkgs.writeShellScriptBin "which-key" ''
  exec ${pkgs.wlr-which-key}/bin/wlr-which-key ${pkgs.writeText "config.yaml" menu}
''
