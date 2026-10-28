# NixOS configurations

Three hosts, three flakes, three git submodules:

| flake       | host        |
| ----------- | ----------- |
| `iso.nix`   | installer   |
| `vps.nix`   | vpsfree     |
| `nova.nix`  | laptop      |

Each host flake is fully self-contained: it builds its own
`nixosConfigurations.<host>`, pins its own dependencies in its own
`flake.lock`, and keeps its own modules next to it (`modules.nix`, plus the
host specific files in that repository). The only things they share are
published flakes, e.g. `configuration.nix` for the generic
NixOS/Home Manager config and its overlay; no host flake depends on
`hosts.nix` itself.

hosts.nix only aggregates the three flakes:

```sh
# via the aggregate flake
nixos-rebuild switch --flake ~/src/hosts.nix#nova

# or directly from the host flake
nixos-rebuild switch --flake ~/src/hosts.nix/nova.nix#nova
```

The vps integration test lives next to the vps flake and is exposed as a
flake check; it uses only the vps flake's inputs (including
`configuration.nix`) and never another host flake:

```sh
nix flake check ~/src/hosts.nix/vps.nix
```
