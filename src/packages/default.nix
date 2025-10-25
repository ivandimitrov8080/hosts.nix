{ inputs, system }:
let
  pkgs = import inputs.nixpkgs { inherit system; };
in
{
  swhkd = pkgs.callPackage ./swhkd { };
  ndlm = pkgs.callPackage ./ndlm { };
}
