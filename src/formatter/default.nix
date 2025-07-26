{ inputs }:
let
  system = "x86_64-linux";
in
{
  "${system}" = inputs.nixpkgs.legacyPackages.${system}.nixfmt-tree;
}
