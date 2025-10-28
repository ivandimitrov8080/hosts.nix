{ pkgs }:
let
  inherit (pkgs) callPackage;
  buildMpv = d: (pkgs.stdenv.mkDerivation (d // { passthru.scriptName = d.name; }));
in
pkgs.lib.recurseIntoAttrs {
  hello-world = callPackage ./hello-world { inherit buildMpv; };
}
