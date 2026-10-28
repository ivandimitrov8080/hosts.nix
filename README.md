# NixOS configurations

Three hosts, three flakes, three git submodules:

| flake      | host      |
| ---------- | --------- |
| `iso.nix`  | installer |
| `vps.nix`  | vpsfree   |
| `nova.nix` | laptop    |

Each host flake is fully self-contained: it builds its own
`nixosConfigurations.<host>`, pins its own dependencies in its own
`flake.lock`, and keeps its own data next to it. The only shared thing is the
published `configuration.nix` flake, which holds _structure and policy_:

```
nixpkgs ─┬─ configuration.nix   defaults, meta.* modules, lib, overlay
         │        ▲
         │        │ inputs.configuration.nixosModules.default
   iso.nix ─ nova.nix ─ vps.nix   data only: hostname, keys, disks, users, IPs
         └────────┬──────────┘
              hosts.nix   merge only, no logic
```

## Layers

`configuration.nix` provides, and hosts only ever _configure_:

| module                                                              | what it owns                                                                           |
| ------------------------------------------------------------------- | -------------------------------------------------------------------------------------- |
| `src/nixos/defaults`                                                | host-agnostic defaults (aliases, shells, nginx, openssh, ...)                          |
| `meta.host`                                                         | per-host data: `stateVersion`, public address                                          |
| `meta.minimal`                                                      | the developer baseline (fonts, nix-ld, kmonad, `meta.{graphicalBoot,shells,swayland}`) |
| `meta.wireguard`                                                    | derives the wg0 systemd-networkd config from a peer record                             |
| `meta.dns`                                                          | nameservers, `services.resolved`, dnscrypt-proxy, DNS egress filter                    |
| `meta.graphics`                                                     | `hardware.graphics`, base `xdg.portal`                                                 |
| `meta.{graphicalBoot,shells,swayland,hyprland,mail,bingwp,grafana}` | opt-in feature bundles                                                                 |
| `src/lib/data.nix`                                                  | values shared by the NixOS and Home Manager trees                                      |

A host repo is therefore only _data_: a `peers.nix` with public wireguard
keys, a `host.nix` with the `meta.*` block, and the genuinely host-specific
files (`machine.nix`, `mail.nix`, `vpsadminos.nix`, ...).

Rules that keep the three repos independent:

1. `configuration.nix` never mentions a hostname, IP, key or host-only package.
2. Hosts never import each other; the vps integration test only imports
   `inputs.configuration` and its own files.
3. Anything a host may need to change is an option under `meta.*`; the shared
   defaults are `mkDefault`ed, so hosts never need `mkForce` (except in tests).

## Usage

```sh
# via the aggregate flake
nixos-rebuild switch --flake ~/src/hosts.nix#nova

# or directly from the host flake
nixos-rebuild switch --flake ~/src/hosts.nix/nova.nix#nova

# evaluation-only check of every host and of the vps integration test
nix flake check ~/src/hosts.nix

# actually run the vps VM integration test
nix build ~/src/hosts.nix/vps.nix#checks.x86_64-linux.integration
```

The vps integration test lives next to the vps flake and is exposed as a
flake check; it uses only the vps flake's inputs (including
`configuration.nix`) and never another host flake. The aggregate flake checks
that it evaluates, but does not run the VMs; run it with the command above.

## Adding something

- New generic feature → module or default in `configuration.nix`, gated by a
  `meta.*.enable` option.
- New host → new flake with its own `flake.lock`, its own `peers.nix` and a
  `host.nix` that only sets `meta.*`; add it to `hosts.nix`'s outputs.
