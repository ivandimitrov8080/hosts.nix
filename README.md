# NixOS configurations

Three hosts, three flakes, three git submodules:

| flake       | host        |
| ----------- | ----------- |
| `iso.nix`   | installer   |
| `vps.nix`   | vpsfree     |
| `nova.nix`  | laptop      |

Each host flake is self-contained: it builds its own
`nixosConfigurations.<host>` and pins its own dependencies in its own
`flake.lock`. The only thing they share is `src/` (modules, overlays and
packages), which every host flake consumes as a `src` flake input, so nothing
is duplicated.

hosts.nix only aggregates the three flakes:

```sh
# via the aggregate flake
nixos-rebuild switch --flake ~/src/hosts.nix#nova

# or directly from the host flake
nixos-rebuild switch --flake ~/src/hosts.nix/nova.nix#nova
```
