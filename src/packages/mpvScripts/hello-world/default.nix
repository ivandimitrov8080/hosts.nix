{ pkgs, buildMpv }:
buildMpv {
  name = "hello-world.js";
  src = ./.;
  installPhase = ''
    mkdir -p $out/share/mpv/scripts/
    cp hello-world.js $out/share/mpv/scripts/
  '';
  meta = {
    description = "Hello World mpv JS script";
    license = pkgs.lib.licenses.mit;
  };
}
