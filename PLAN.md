# Refactor plan: one owner per piece of configuration

Goal: no duplicated configuration between `iso.nix`, `nova.nix`, `vps.nix` and
`configuration.nix`, while each host stays self-contained and independently
deployable with `configuration.nix` as its only shared dependency, and
`hosts.nix` stays a pure aggregator.

## Invariants

1. A host flake evaluates alone: `nix flake check path:./nova.nix` works with
   no `hosts.nix` and no sibling submodule checked out.
2. Hosts only depend on `inputs.configuration` (plus their own inputs).
3. `configuration.nix` contains no host names, IPs, keys or host-only packages.
4. Everything a host may want to change is a `meta.*` option; shared values are
   `mkDefault`ed so hosts never need `mkForce` (tests excepted).
5. `hosts.nix` merges `nixosConfigurations` and adds eval-only checks.

## Phases

### 0 — Safety net

- Per host: `nix eval .#nixosConfigurations.<host>.config.system.build.toplevel.drvPath`.
- Aggregator: `checks.<system>.<host>` evaluate the toplevel `drvPath` without
  building it (`builtins.unsafeDiscardStringContext`).
- `vps.nix/test` is the behavioural regression net for wireguard/DNS/nftables.
- Guardrail worth adding later: `grep -rniE 'nova|vpsfree|10\.0\.0\.|37\.205' configuration.nix/src`
  must stay empty.
- Iterate without pushing: `nix build --override-input configuration path:../configuration.nix`.

### 1 — Kill the per-host `default` prologue

Was: identical ~30 lines in all three `modules.nix` (`imports`,
`nix.registry`, `nixpkgs.hostPlatform`, overlay list,
`system.stateVersion`, `users.defaultUserShell`).

Now: `meta.host` in `configuration.nix` owns all of it, hosts pass data only
(`meta.host.overlays`, `meta.host.platform`) and their flake inputs through
`specialArgs`. `meta.host.registry.enable` defaults to `inputs ? self`, so the
same module still evaluates without flake inputs (vps test).

### 2 — One `meta.minimal`

Was: two ~120 line copies (iso, nova) differing only in `OPTIONS_JSON_*`.
Now: `meta.minimal` + `meta.minimal.kmonad.{enable,device,config}` +
`meta.minimal.optionDocs.{nixos,homeManager}`.

### 3 — Wireguard and DNS: one owner per option

- `meta.wireguard.peers` is a record (`ip`, `publicKey`, `endpoint`,
  `persistentKeepalive`) plus `meta.wireguard.self`; `AllowedIPs`, hub/spoke
  layout and per-link DNS are derived. Drops the `isHub`/`isCurrent` address
  guessing.
- `meta.dns` owns `networking.nameservers`, `services.resolved`,
  `services.dnscrypt-proxy` and the DNS egress nftables ruleset. The old
  `meta.dnscrypt` module (hardcoded `10.0.0.1`) is gone, and the wireguard
  module no longer touches DNS at all.
- Host data left behind: a `peers.nix` per host (public keys + addresses).

### 4 — Shared values inside `configuration.nix`

- `src/lib/data.nix`: shell aliases, zoxide aliases, eza flags/aliases, zsh
  constants. Consumed by both `src/nixos/defaults` and
  `src/homeManager/modules`, which otherwise duplicate them.
- `meta.graphics` owns `hardware.graphics` + the base `xdg.portal`;
  `meta.swayland`/`meta.hyprland` only add their own portal.
- Home Manager no longer hardcodes a Firefox MIME default (each host picks its
  own browser); `home.pointerCursor.enable` is set once in the shared module.

### 5 — Per-host cleanups

- nova: `packages/emacsPackages/domains.nix` is the single domain list; the
  Emacs package set and `extraEmacsPackages` derive from it. `catppuccin`
  settings defined once, reused for the system and the user session.
- vps: `test/default.nix` uses `mkInternetNode`/`wgModule` helpers and the
  shared `meta.dns`/`meta.wireguard` modules instead of hand-rolled copies.
- iso: `host.nix` is just the installer bits plus `meta.minimal.enable`.
- All three `modules.nix` files are deleted.

### 6 — Migration order (for a future cross-repo change)

1. `configuration.nix`: add the option/module, keep the old code working.
2. Push; in each host `nix flake update configuration`.
3. Host adopts the `meta.*` options, deletes its local copy.
4. `configuration.nix`: delete the now-unused code.
5. `hosts.nix`: bump submodule pointers, `nix flake check`.

Never delete before all hosts migrated — hosts pin `configuration.nix` by lock.

## Verification

```sh
nix flake check ~/src/hosts.nix            # eval-only checks for all hosts
nix flake check ~/src/hosts.nix/vps.nix    # integration test
nix build --dry-run ~/src/hosts.nix/iso.nix#nixosConfigurations.iso.config.system.build.isoImage
cp -r ~/src/hosts.nix/nova.nix /tmp/isolation && nix flake check /tmp/isolation
```

## Deliberately still duplicated

- Wireguard peer keys/IPs, hostnames, user hashes, disk UUIDs, public
  endpoints: host data, and sharing it would break self-containment.
- The `nixosSystem { ... }` invocation per host: that *is* the deployment entry
  point.
- Three `flake.lock` nixpkgs pins; `updater.nu` keeps them aligned.

## Not done yet

- `meta.user.defaultGroups`: the 12-group user baseline is still spelled out in
  `iso.nix` and `nova.nix` (vps needs a different set via `mkForce`).
- `emacs.el` still lists its `(require ...)` calls by hand, because the order
  there is load order; adding a domain is two edits instead of three.
- Home Manager's `backupFileExtension`/`useUserPackages`/`useGlobalPkgs` trio is
  still three lines per host; sharing it would couple `configuration.nix` to
  the home-manager input.
