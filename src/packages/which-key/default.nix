{ pkgs, lib, ... }:
let
  menu = lib.generators.toYAML { } {
    anchor = "center";
    menu = [
      {
        key = "p";
        desc = "Power";
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
    ];
  };
in
pkgs.writeShellScriptBin "which-key" ''
  exec ${pkgs.wlr-which-key}/bin/wlr-which-key ${pkgs.writeText "config.yaml" menu}
''
