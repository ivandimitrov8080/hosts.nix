def --wrapped "main shell" [ref: string, flake_path: string = /home/ivand/src/hosts.nix, ...rest] { nix develop $"($flake_path)#($ref)" ...$rest }
def main [] { $"Usage: todo..." }
