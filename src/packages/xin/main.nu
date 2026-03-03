const flake_path = "/home/ivand/src/hosts.nix"
def shells [] {
    nix flake show --json $flake_path | from json | get devShells.x86_64-linux | transpose name info | get name
}
def hosts [] {
    nix flake show --json $flake_path | from json | get nixosConfigurations | transpose name info | get name
}
export def --wrapped shell [ref: string@shells, ...rest] { nix develop $"($flake_path)#($ref)" ...$rest }
export def --wrapped switch [ref: string@hosts, ...rest] { nixos-rebuild switch --flake $"($flake_path)#($ref)" --profile-name $ref --sudo --ask-sudo-password ...$rest }
