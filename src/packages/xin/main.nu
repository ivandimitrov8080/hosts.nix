const flake_path = "/home/ivand/src/hosts.nix"
export def --wrapped shell [ref: string, ...rest] { nix develop $"($flake_path)#($ref)" ...$rest }
export def --wrapped switch [ref: string, ...rest] { nixos-rebuild switch --flake $"($flake_path)#($ref)" --profile-name $ref --sudo --ask-sudo-password ...$rest }
export def main [] { $"Usage: todo..." }
