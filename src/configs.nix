# The hosts are split into three flakes, each a git submodule of this repo:
#
#   ../iso.nix   -> nixosConfigurations.iso
#   ../vps.nix   -> nixosConfigurations.vps
#   ../nova.nix  -> nixosConfigurations.nova
#
# ./modules, ./overlays.nix and ./packages are shared code that every host
# flake consumes as the `src` flake input (this directory), so each host flake
# is self-contained and can be evaluated on its own.
