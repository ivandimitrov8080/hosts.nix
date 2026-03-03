let flake_path = "/home/ivand/src/hosts.nix"
def --wrapped "main shell" [ref: string, ...rest] { nix develop $"($flake_path)#($ref)" ...$rest }
def --wrapped "main switch" [ref: string, ...rest] { nixos-rebuild switch --flake $"($flake_path)#($ref)" --profile-name $ref --sudo --ask-sudo-password ...$rest }
def main [] { $"Usage: todo..." }
