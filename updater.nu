let cmds = [
    "nix flake update"
    "nix flake check --abort-on-warn"
    "git add ."
]
$cmds | each {|it| git submodule foreach $it }
