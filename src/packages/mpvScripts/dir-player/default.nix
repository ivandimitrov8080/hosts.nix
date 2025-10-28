{ buildMpv }:
buildMpv {
  name = "dir-player.lua";
  src = ./.;
  meta = {
    description = ''
      Dir player mpv script.
      Provide a dir and it plays all supported formats while remembering where it last stopped.
    '';
  };
}
