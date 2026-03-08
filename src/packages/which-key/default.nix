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
    exec = "nu";
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
            cmd = # nu
              ''
                ps | select pid name | (to tsv -n | rofi -dmenu -i -p "ee") | from tsv -n | get 0.column0 | kill -9 $in
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
                cmd = # nu
                  ''
                    rofi -dmenu -i -p "Volume %" | wpctl set-volume @DEFAULT_AUDIO_SOURCE@ $"($in)%"
                  '';
              }
              {
                key = "t";
                desc = "Toggle Mute";
                cmd = # nu
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
                cmd = # nu
                  ''
                    rofi -dmenu -i -p "Volume %" | wpctl set-volume @DEFAULT_AUDIO_SINK@ $"($in)%"
                  '';
              }
              {
                key = "t";
                desc = "Toggle Mute";
                cmd = # nu
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
            cmd = # nu
              ''
                rofi -dmenu -i -p "Brightness %" | brightnessctl set $"($in)%"
              '';
          }
        ];
      }
      {
        key = "i";
        desc = "Inputs";
        submenu = [
          {
            key = "t";
            desc = "Toggle";
            cmd = # nu
              ''
                swaymsg -t get_inputs --raw | from json | select identifier name type | (let rows = $in; let idx = ($rows | select type name | to tsv -n | rofi -dmenu -i -p "Toggle Device" -format i | into int); ($rows | get $idx)) |  get identifier | swaymsg input $in events toggle
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
      rev = "4227842b6fbfd25d843398aed7181217acd5fa9e";
      hash = "sha256-FR9sYSdyi2OwKBbrkCtMcrOnMIY1m/bxbU7jdBClMGI=";
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
