{ pkgs }:
let
  inherit (pkgs) callPackage;
  buildMpv =
    d:
    (pkgs.stdenv.mkDerivation (
      d
      // {
        installPhase = ''
          mkdir -p $out/share/mpv/scripts/
          cp ./*.lua $out/share/mpv/scripts/
        '';
        meta.license = pkgs.lib.licenses.mit;
        passthru.scriptName = d.name;
      }
    ));
in
pkgs.lib.recurseIntoAttrs {
  dir-player = callPackage ./dir-player { inherit buildMpv; };
}
