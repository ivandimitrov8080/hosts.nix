let repos = ["idimitrov.dev", "ndlm", "metronome", "hosts.nix"]
def update_flake [flake_path: string] {
    cd $flake_path
    mut result = []
    mut err = false
    let commands = [
        { nix flake update }
        { nix flake check --abort-on-warn }
        { git reset }
        { git add flake.lock }
        { git -c commit.gpgsign=false commit -m "auto-update" }
        { git push }
    ]
    for cmd in $commands {
        let res = do $cmd | complete | merge {cmd: (view source $cmd)}
        let e = $err or $res.exit_code != 0
        $result = $result | append $res
        $err = $e
        if $e { break }
    }
    return {
        result: $result
        err: $err
        flake_path: $flake_path
    }
}
$repos | each {|r| update_flake $"~/src/($r)" }
