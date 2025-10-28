{ pkgs }:
let
  inherit (pkgs) callPackage;
  buildMpv = d: (pkgs.stdenv.mkDerivation (d // { passthru.scriptName = d.name; }));
in
pkgs.lib.recurseIntoAttrs {
  dir-player = callPackage ./dir-player { inherit buildMpv; };
}
