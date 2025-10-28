{ pkgs, buildMpv }:
buildMpv rec {
  name = "dir-player.js";
  src = ./.;
  installPhase = ''
    mkdir -p $out/share/mpv/scripts/
    cp ${name} $out/share/mpv/scripts/
  '';
  meta = {
    description = ''
      Dir player mpv script.
      Provide a dir and it plays all supported formats while remembering where it last stopped.
    '';
    license = pkgs.lib.licenses.mit;
  };
}
