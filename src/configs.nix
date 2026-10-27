# The hosts are split into three flakes, each a git submodule of this repo:
#
#   ../iso.nix   -> nixosConfigurations.iso
#   ../vps.nix   -> nixosConfigurations.vps
#   ../nova.nix  -> nixosConfigurations.nova
#
# ./modules, ./overlays.nix and ./packages are shared code that the host
# flakes import (hosts.nix hands them this directory as `src`).
