# NixOS configurations

Three hosts, three flakes, three git submodules:

| flake       | host        |
| ----------- | ----------- |
| `iso.nix`   | installer   |
| `vps.nix`   | vpsfree     |
| `nova.nix`  | laptop      |

`src/` holds the code shared by all of them (`modules`, `overlays.nix`,
`packages`). hosts.nix only depends on the three flakes and hands them `src/`,
so nothing is duplicated and each host pins its own dependencies in its own
`flake.lock`.

```sh
nixos-rebuild switch --flake ~/src/hosts.nix#nova
```
